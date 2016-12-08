{-# LANGUAGE StandaloneDeriving #-}

-- | SSC-related serialization

module Pos.Binary.Ssc where

import           Universum

import           Pos.Binary.Class              (Bi (..))
import           Pos.Binary.Crypto             ()
import           Pos.Ssc.GodTossing.Types.Base (Commitment (..), VssCertificate (..))

-- hayaku hayaku! impuremento kore no inusutanso kudasai!
instance Bi Commitment where
    put = notImplemented
    get = notImplemented

instance Bi VssCertificate where
    put = notImplemented
    get = notImplemented
