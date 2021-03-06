{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Api.Typed.Orphans () where

import           Cardano.Prelude

import           Cardano.Api.Typed
import           Cardano.Crypto.Hash hiding (Hash)
import           Cardano.Crypto.KES

import           Test.Cardano.Api.Orphans ()

-- Address instances

deriving instance Eq (Address Byron)
deriving instance Show (Address Byron)

deriving instance Eq (Address Shelley)
deriving instance Show (Address Shelley)

deriving instance Eq StakeAddress
deriving instance Show StakeAddress


-- Signing Key instances

deriving instance Eq (SigningKey ByronKey)
deriving instance Eq (SigningKey PaymentKey)
deriving instance Eq (SigningKey StakeKey)
deriving instance Eq (SigningKey StakePoolKey)
deriving instance Eq (SigningKey GenesisKey)
deriving instance Eq (SigningKey GenesisDelegateKey)
deriving instance Eq (SigningKey GenesisUTxOKey)
deriving instance Eq (SigningKey KesKey)
deriving instance Eq (SigningKey VrfKey)


instance (HashAlgorithm h, KESAlgorithm d) => Eq (SignKeyKES (SumKES h d)) where
  k1 == k2 = rawSerialiseSignKeyKES k1 == rawSerialiseSignKeyKES k2
