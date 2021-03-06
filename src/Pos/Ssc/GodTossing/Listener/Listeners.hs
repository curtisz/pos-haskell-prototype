{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Instance of SscListenersClass

module Pos.Ssc.GodTossing.Listener.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscGodTossing
       ) where

import           Data.HashMap.Strict                    (lookup)
import           Data.List.NonEmpty                     (NonEmpty)
import           Data.Tagged                            (Tagged (..))
import           Formatting                             (build, sformat, stext, (%))
import           System.Wlog                            (logDebug, logInfo)
import           Universum

import           Pos.Communication.Methods              (sendToNeighborsSafe)
import           Pos.Communication.Types                (ResponseMode)
import           Pos.DHT                                (ListenerDHT (..), replyToNode)
import           Pos.Slotting                           (getCurrentSlot)
import           Pos.Ssc.Class.Listeners                (SscListenersClass (..))
import           Pos.Ssc.Class.LocalData                (sscGetLocalPayload)
import           Pos.Ssc.GodTossing.LocalData.LocalData (sscIsDataUseful,
                                                         sscProcessMessage)
import           Pos.Ssc.GodTossing.Types.Instance      ()
import           Pos.Ssc.GodTossing.Types.Message       (DataMsg (..), InvMsg (..),
                                                         MsgTag (..), ReqMsg (..),
                                                         dataMsgPublicKey, dataMsgTag,
                                                         isGoodSlotIdForTag)
import           Pos.Ssc.GodTossing.Types.Type          (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types         (GtPayload (..), _gpCertificates)
import           Pos.Types                              (Address)
import           Pos.WorkMode                           (WorkMode)

instance SscListenersClass SscGodTossing where
    sscListeners =
        Tagged
            [ ListenerDHT handleInv
            , ListenerDHT handleReq
            , ListenerDHT handleData
            ]

handleInv :: ResponseMode SscGodTossing m => InvMsg -> m ()
handleInv (InvMsg tag keys) =
    ifM (isGoodSlotIdForTag tag <$> getCurrentSlot)
        (handleInvDo tag keys)
        (logDebug $
         sformat ("Ignoring "%build%", because slot is not appropriate") tag)

handleInvDo :: ResponseMode SscGodTossing m => MsgTag -> NonEmpty Address -> m ()
handleInvDo tag keys = mapM_ handleSingle keys
  where
    handleSingle addr =
        ifM (sscIsDataUseful tag addr)
            (replyToNode $ ReqMsg tag addr)
            (logDebug $
             sformat ("Ignoring "%build% " ("%build%"), because it's useless")
                 tag addr)

handleReq :: ResponseMode SscGodTossing m => ReqMsg -> m ()
handleReq (ReqMsg tag addr) = do
    localPayload <- sscGetLocalPayload =<< getCurrentSlot
    whenJust (toDataMsg tag addr localPayload) (replyToNode @_ @DataMsg)

toDataMsg :: MsgTag -> Address -> GtPayload -> Maybe DataMsg
toDataMsg CommitmentMsg addr (CommitmentsPayload comm _) =
    DMCommitment addr <$> lookup addr comm
toDataMsg OpeningMsg addr (OpeningsPayload opens _) =
    DMOpening addr <$> lookup addr opens
toDataMsg SharesMsg addr (SharesPayload shares _) =
    DMShares addr <$> lookup addr shares
toDataMsg VssCertificateMsg addr payload =
    DMVssCertificate addr <$> lookup addr (_gpCertificates payload)
toDataMsg _ _ _ = Nothing

handleData :: WorkMode SscGodTossing m => DataMsg -> m ()
handleData msg =
    whenM (isGoodSlotIdForTag (dataMsgTag msg) <$> getCurrentSlot) $
    do added <- sscProcessMessage msg
       let tag = dataMsgTag msg
           addr = dataMsgPublicKey msg
       loggerAction tag added addr
       when added $ sendToNeighborsSafe $ InvMsg tag $ pure addr

loggerAction :: WorkMode SscGodTossing m
             => MsgTag -> Bool -> Address -> m ()
loggerAction msgTag added addr = logAction msg
  where
      msgAction | added = "added to local storage"
                | otherwise = "ignored"
      msg = sformat (build%" from "%build%" have/has been "%stext)
          msgTag addr msgAction
      logAction | added = logInfo
                | otherwise = logDebug
