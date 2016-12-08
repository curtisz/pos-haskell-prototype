-- | For Pos.Types.* modules

module Pos.Binary.Types where

import           Universum

import           Pos.Binary.Class    (Bi (..))
import           Pos.Types.Timestamp (Timestamp (..))

instance Bi Timestamp where
  get = fromInteger <$> get
  put = put . toInteger
