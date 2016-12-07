module Main where

import           Control.Monad.Trans.Resource (runResourceT)
import           Universum

import           Pos.State.Modern             (Patak (..), getPatak, openNodeDB, putPatak,
                                               runNodeDBHolder)

main :: IO ()
main =
    runResourceT $
    do db <- openNodeDB "vasya utkin"
       runNodeDBHolder db $
           do print =<< getPatak
              putPatak $ Patak "bardaq"
              print =<< getPatak
