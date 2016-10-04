-- | Methods which use communication.

module Pos.Communication.Methods
       ( fullNode
       , inSlot
       , node_ping
       , systemStart
       ) where

import qualified Data.Binary              as Bin (encode)

import           Data.Fixed               (div')
import           Data.IORef               (IORef, newIORef, readIORef)
import qualified Data.Map                 as Map ((!))
import qualified Data.Text                as T
import           Formatting               (build, int, sformat, (%))
import           System.IO.Unsafe         (unsafePerformIO)
import           System.Random            (randomIO, randomRIO)
import           Universum

import           Control.TimeWarp.Logging (LoggerName (..), logError, logInfo,
                                           setLoggerName)
import           Control.TimeWarp.Timed   (Microsecond, for, fork_, ms, repeatForever,
                                           sec, till, virtualTime, wait)
import           Serokell.Util            ()

import           Pos.Communication.Types  (Message (..), Node)
import           Pos.Constants            (epochSlots, slotDuration, t)
import           Pos.Crypto               (encrypt, hashRaw, shareSecret)
import           Pos.State.Operations     (addEntry, addLeaders, adoptBlock, createBlock,
                                           getLeader, getLeaders, mkNodeState, setLeaders)
import           Pos.Types.Types          (Entry (..), NodeId (..), displayEntry, nodeF)
import           Pos.WorkMode             (WorkMode)

----------------------------------------------------------------------------
-- Network simulation
----------------------------------------------------------------------------

node_ping :: WorkMode m => NodeId -> Node m
node_ping pingId = \_self _keypair _n _keys sendTo -> do
    inSlot True $ \_epoch _slot -> do
        logInfo $ sformat ("pinging "%nodeF) pingId
        sendTo pingId MPing
    return $ \n_from message -> case message of
        MPing -> do
            logInfo $ sformat ("pinged by "%nodeF) n_from
        _ -> do
            logInfo $ sformat ("unknown message from "%nodeF) n_from

systemStart :: IORef Microsecond
systemStart = unsafePerformIO $
    newIORef (panic "systemStart: accessed before it was written")
{-# NOINLINE systemStart #-}

{- |
Run something at the beginning of every slot. The first parameter is epoch
number (starting from 0) and the second parameter is slot number in the epoch
(from 0 to epochLen-1).

The 'Bool' parameter says whether a delay should be introduced. It's useful
for nodes (so that node logging messages would come after “EPOCH n” logging
messages).
-}
inSlot :: WorkMode m => Bool -> (Int -> Int -> m ()) -> m ()
inSlot extraDelay f = fork_ $ do
    start <- liftIO $ readIORef systemStart
    let getAbsoluteSlot :: WorkMode m => m Int
        getAbsoluteSlot = do
            now <- virtualTime
            return (div' (now - start) slotDuration)
    -- Wait until the next slot begins
    nextSlotStart <- do
        absoluteSlot <- getAbsoluteSlot
        return (start + fromIntegral (absoluteSlot+1) * slotDuration)
    -- Now that we're synchronised with slots, start repeating
    -- forever. 'repeatForever' has slight precision problems, so we delay
    -- everything by 50ms.
    wait (till nextSlotStart)
    repeatForever slotDuration handler $ do
        wait (for 50 ms)
        when extraDelay $ wait (for 50 ms)
        absoluteSlot <- getAbsoluteSlot
        let (epoch, slot) = absoluteSlot `divMod` epochSlots
        f epoch slot
  where
    handler e = do
        logError $ sformat
            ("error was caught, restarting in 5 seconds: "%build) e
        return $ sec 5

{- ==================== TODO ====================

Timing issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* What to do about blocks delivered a bit late? E.g. let's assume that a
  block was generated in slot X, but received by another node in slot Y. What
  are the conditions on Y under which the block should (and shouldn't) be
  accepted?

* Let's say that we receive a transaction, and then we receive a block
  containing that transaction. We remove the transaction from our list of
  pending transactions. Later (before K slots pass) it turns out that that
  block was bad, and we discard it; then we should add the transaction
  back. Right? If this is how it works, then it means that somebody can
  prevent the transaction from being included into the blockchain for the
  duration of K−1 slots – right? How easy/probable/important is it in
  practice?

Validation issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Blocks should build on each other. We should discard shorter histories.

* We should validate entries that we receive

* We should validate blocks that we receive; in particular, we should check
  that blocks we receive are generated by nodes who had the right to generate
  them

Other issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Create a typo synonym for epoch number?

* We should be able to query blocks from other nodes, like in Bitcoin (if
  e.g. we've been offline for several slots or even epochs) but this isn't
  implemented yet. In fact, most stuff from Bitcoin isn't implemented.

* We should exclude extremely delayed entries that are the same as ones we
  already received before, but already included into one of the previous
  blocks.

-}

{-
If some node becomes inactive, other nodes will be able to recover its U by
exchanging decrypted pieces of secret-shared U they've been sent.

After K slots all nodes are guaranteed to have a common prefix; each node
computes the random satoshi index from all available Us to find out who has
won the leader election and can generate the next block.
-}

fullNode :: WorkMode m => Node m
fullNode = \self _key n keys sendTo ->
  setLoggerName (LoggerName (toS (sformat nodeF self))) $ do
    nodeState <- mkNodeState

    -- This will run at the beginning of each slot:
    inSlot True $ \epoch slot -> do
        -- For now we just send messages to everyone instead of letting them
        -- propagate, implementing peers, etc.
        let sendEveryone x = for_ [NodeId 0 .. NodeId (n-1)] $ \i ->
                                 sendTo i x

        -- Create a block and send it to everyone
        let createAndSendBlock = do
                blk <- createBlock nodeState
                sendEveryone (MBlock blk)
                if null blk then
                    logInfo "created an empty block"
                else
                    logInfo $ T.intercalate "\n" $
                        "created a block:" :
                        map (\e -> "  * " <> displayEntry e) blk

        -- If this is the first epoch ever, we haven't agreed on who will
        -- mine blocks in this epoch, so let's just say that the 0th node is
        -- the master node. In slot 0, node 0 will announce who will mine
        -- blocks in the next epoch; in other slots it will just mine new
        -- blocks.
        when (self == NodeId 0 && epoch == 0) $ do
            when (slot == 0) $ do
                leaders <- map NodeId <$>
                           replicateM epochSlots (liftIO $ randomRIO (0, n-1))
                addLeaders nodeState epoch leaders
                logInfo "generated random leaders for epoch 1 \
                        \(as master node)"
            createAndSendBlock

        -- When the epoch starts, we do the following:
        --   * generate U, a random bitvector that will be used as a seed to
        --     the PRNG that will choose leaders (nodes who will mine each
        --     block in the next epoch). For now the seed is actually just a
        --     Word64.
        --   * secret-share U and encrypt each piece with corresponding
        --     node's pubkey; the secret can be recovered with at least
        --     N−T available pieces
        --   * post encrypted shares and a commitment to U to the blockchain
        --     (so that later on we wouldn't be able to cheat by using
        --     a different U)
        when (slot == 0) $ do
            u <- liftIO (randomIO :: IO Word64)
            let (_, shares) = shareSecret n (n - t) (toS (Bin.encode u))
            for_ (zip shares [NodeId 0..]) $ \(share, i) -> do
                encShare <- encrypt (keys Map.! i) share
                sendEveryone (MEntry (EUShare self i encShare))
            sendEveryone (MEntry $ EUHash self $ hashRaw $ toS $ Bin.encode u)

        -- If we are the epoch leader, we should generate a block
        do leader <- getLeader nodeState epoch slot
           when (leader == Just self) $
               createAndSendBlock

        -- According to @gromak (who isn't sure about this, but neither am I):
        -- input-output-rnd.slack.com/archives/paper-pos/p1474991379000006
        --
        -- > We send commitments during the first slot and they are put into
        -- the first block. Then we wait for K periods so that all nodes
        -- agree upon the same first block. But we see that it’s not enough
        -- because they can agree upon dishonest block. That’s why we need to
        -- wait for K more blocks. So all this *commitment* phase takes 2K
        -- blocks.

    -- This is our message handling function:
    return $ \n_from message -> case message of
        -- An entry has been received: add it to the list of unprocessed
        -- entries
        MEntry e -> addEntry nodeState e

        -- A block has been received: remove all pending entries we have
        -- that are in this block, then add the block to our local
        -- blockchain and use info from the block
        MBlock es -> do
            adoptBlock nodeState es
            -- TODO: using withNodeState several times here might break
            -- atomicity, I dunno
            for_ es $ \e -> case e of
                ELeaders epoch leaders -> do
                    mbLeaders <- getLeaders nodeState epoch
                    case mbLeaders of
                        Nothing -> setLeaders nodeState epoch leaders
                        Just _  -> logError $ sformat
                            (nodeF%" we already know leaders for epoch "%int
                                  %"but we received a block with ELeaders "
                                  %"for the same epoch") self epoch
                -- TODO: process other types of entries
                _ -> return ()

        -- We were pinged
        MPing -> logInfo $ sformat
                     ("received a ping from "%nodeF) n_from