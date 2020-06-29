{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.TextEnvelope.Golden.Tests
  ( tests
  ) where

import           Cardano.Prelude

import           Test.CLI.Shelley.TextEnvelope.Golden.GenesisDelegateKeys (golden_shelleyGenesisDelegateKeys)
import           Test.CLI.Shelley.TextEnvelope.Golden.GenesisKeys (golden_shelleyGenesisKeys)
import           Test.CLI.Shelley.TextEnvelope.Golden.PaymentKeys (golden_shelleyPaymentKeys)
import           Test.CLI.Shelley.TextEnvelope.Golden.StakeKeys (golden_shelleyStakeKeys)

import qualified Hedgehog as H

tests :: IO Bool
tests =
  H.checkSequential
    $ H.Group "TextEnvelopeGoldens"
        [ ("golden_shelleyPaymentKeys", golden_shelleyPaymentKeys)
        , ("golden_shelleyStakeKeys", golden_shelleyStakeKeys)
        , ("golden_shelleyGenesisKeys", golden_shelleyGenesisKeys)
        , ("golden_shelleyGenesisDelegateKeys", golden_shelleyGenesisDelegateKeys)
        ]
