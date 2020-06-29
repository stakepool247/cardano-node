{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.Shelley.TextEnvelope.Golden.GenesisDelegateKeys
  ( golden_shelleyGenesisDelegateKeys
  ) where

import           Cardano.Prelude

import           Data.Proxy (Proxy(..))

import           Cardano.Api.Typed (AsType, GenesisDelegateKey,
                   HasTextEnvelope (..), HasTypeProxy(..),
                   OperationalCertificateIssueCounter,
                   SigningKey, VerificationKey)

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.OptParse


-- | 1. We generate a key pair & operational certificate counter file
--   2. We check for the existence of the key pair & counter file
--   3. We check the TextEnvelope serialization format has not changed.
golden_shelleyGenesisDelegateKeys :: Property
golden_shelleyGenesisDelegateKeys =
  propertyOnce $ do

    -- Reference keys
    let reference_ver_key = "test/Test/golden/shelley/genesis_delegate_keys/verification_key"
        reference_sign_key = "test/Test/golden/shelley/genesis_delegate_keys/signing_key"
        reference_op_cert_counter = "test/Test/golden/shelley/genesis_delegate_keys/operational_certificate_counter"

    -- Key filepaths
    let verKey = "genesis-delegate-verification-key-file"
        signKey = "genesis-delegate-signing-key-file"
        opCertCounter = "delegate-operational-cert-counter-file"
        createdFiles = [opCertCounter, verKey, signKey]

    -- Generate payment verification key
    execCardanoCLIParser
      createdFiles
      "golden_shelleyGenesisDelegateKeys.genesis_delegate_keypair_gen"
        $ evalCardanoCLIParser [ "shelley","genesis","key-gen-delegate"
                               , "--verification-key-file", verKey
                               , "--signing-key-file", signKey
                               , "--operational-certificate-issue-counter-file", opCertCounter
                               ]

    doFilesExist createdFiles


    let signingKeyType = textEnvelopeType (proxyToAsType Proxy :: AsType (SigningKey GenesisDelegateKey))
        verificationKeyType = textEnvelopeType (proxyToAsType Proxy :: AsType (VerificationKey GenesisDelegateKey))
        operationalCertCounterType = textEnvelopeType (proxyToAsType Proxy :: AsType OperationalCertificateIssueCounter)

    -- Check the newly created files have not deviated from the
    -- golden files
    checkTextEnvelopeFormat createdFiles verificationKeyType reference_ver_key verKey
    checkTextEnvelopeFormat createdFiles signingKeyType reference_sign_key signKey
    checkTextEnvelopeFormat createdFiles operationalCertCounterType reference_op_cert_counter opCertCounter

    liftIO $ fileCleanup createdFiles
    H.success
