{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.TextEnvelope.Golden.PaymentKeys
  ( golden_shelleyPaymentKeys
  ) where

import           Cardano.Prelude

import           Data.Proxy (Proxy(..))

import           Cardano.Api.Typed (AsType, HasTextEnvelope (..),
                   HasTypeProxy(..), PaymentKey, SigningKey, VerificationKey)

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. We generate a key pair
--   2. We check for the existence of the key pair
--   3. We check the TextEnvelope serialization format has not changed.
golden_shelleyPaymentKeys :: Property
golden_shelleyPaymentKeys =
  propertyOnce $ do

    -- Reference keys
    let reference_ver_key = "test/Test/golden/shelley/payment_keys/verification_key"
        reference_sign_key = "test/Test/golden/shelley/payment_keys/signing_key"

    -- Key filepaths
    let verKey = "address-verification-key-file"
        signKey = "address-signing-key-file"
        createdFiles = [verKey, signKey]

    -- Generate payment verification key
    execCardanoCLIParser
      createdFiles
      "golden_shelleyPaymentKeys.payment_keypair_gen"
        $ evalCardanoCLIParser [ "shelley","address","key-gen"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               ]

    doFilesExist createdFiles


    let signingKeyType = textEnvelopeType (proxyToAsType Proxy :: AsType (SigningKey PaymentKey))
        verificationKeyType = textEnvelopeType (proxyToAsType Proxy :: AsType (VerificationKey PaymentKey))

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles verificationKeyType reference_ver_key verKey
    checkTextEnvelopeFormat createdFiles signingKeyType reference_sign_key signKey

    liftIO $ fileCleanup createdFiles
    H.success
