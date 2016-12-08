-- | For Pos.Types.* modules

module Pos.Binary.Types where

import           Pos.Types.Address (Address (..))

instance Bi Address where
    get = do
        ver <- Bi.getWord8
        addrHash <- get
        let addr = PubKeyAddress ver addrHash
            ourChecksum = crc32 addr
        theirChecksum <- Bi.getWord32be
        if theirChecksum /= ourChecksum
            then fail "Address has invalid checksum!"
            else return addr
    put addr@PubKeyAddress {..} = do
        Bi.putWord8 addrVersion
        put addrHash
        Bi.putWord32be $ crc32 addr
