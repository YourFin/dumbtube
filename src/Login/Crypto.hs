{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Login.Crypto where

import Crypto.ECC (Curve_P256R1)
import Crypto.Error (eitherCryptoError)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Number.Serialize (os2ip)
import Crypto.PubKey.ECDSA qualified as ECDSA
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Crypto.PubKey.RSA qualified as RSA
import Data.ByteString qualified as ByteString
import Data.String.Interpolate (i)
import Data.ULID (ULID)
import Optics
import Optics.TH

-- import Crypton.

data SignatureAlg
  = -- | Preferred, available via web crypto api
    Ed25519
  | -- | Widely supported by passkeys, preferred to RS256
    ES256
  | -- | For greater coverage of passkeys
    RS256
  deriving (Eq, Show, Generic)

-- | a > b => Prefer a to b
instance Ord SignatureAlg where
  compare a b
    | a == b = EQ
    | otherwise = case (a, b) of
        (Ed25519, _) -> GT
        (_, Ed25519) -> LT
        (ES256, _) -> GT
        (_, ES256) -> LT
        _ -> EQ

coseId :: ReversedPrism' SignatureAlg Int
coseId = re $ prism toInt fromInt
 where
  toInt RS256 = -257
  toInt Ed25519 = -19
  toInt ES256 = -7

  fromInt :: Int -> Either Int SignatureAlg
  fromInt (-257) = Right RS256
  fromInt (-19) = Right Ed25519
  fromInt (-7) = Right ES256
  fromInt a = Left a

description :: SignatureAlg -> String
-- https://www.rfc-editor.org/rfc/rfc7518#section-3.1
description ES256 = "ECDSA w/ SHA-256"
description Ed25519 = "EdDSA using Ed25519 curve"
description RS256 = "RSASSA-PKCS1-v1_5 using SHA-256"

data Challenge = Challenge
  { algorithm :: SignatureAlg
  , nonce :: ByteString
  , ulid :: ULID
  , pubkey :: ByteString -- TODO
  }
  deriving (Eq, Ord, Show, Generic)

makeFieldLabelsNoPrefix ''Challenge

data ChallengeWire = ChallengeWire
  { algorithm :: SignatureAlg
  , nonce :: ByteString
  , ulid :: ULID
  }
  deriving (Eq, Ord, Show, Generic)

makeFieldLabelsNoPrefix ''ChallengeWire

{- | Note: public key and nonce not included on
  purpose.
-}
data Response = Response
  { ulid :: ULID
  , signature :: ByteString
  }
  deriving (Eq, Ord, Show, Generic)

makeFieldLabelsNoPrefix ''Response

newtype ValidationError = ValidationError Text
  deriving stock (Eq, Ord, Show, Generic)

validate :: Challenge -> Response -> Either ValidationError ()
validate challenge resp = _Left %~ ValidationError $ validate' challenge resp

validate' :: Challenge -> Response -> Either Text ()
validate' challenge resp
  | (challenge ^. #ulid) /= (resp ^. #ulid) =
      Left
        [i|Mismatching challenge (#{challenge ^. #ulid}) and response (#{resp ^. #ulid}) IDs|]
  | otherwise = case challenge ^. #algorithm of
      -- TODO
      ES256 -> do
        pubKey <-
          challenge
            ^. #pubkey
            & ECDSA.decodePublic (Proxy @Curve_P256R1)
            & eitherCryptoError
            & first
              ( \cryptoErr ->
                  [i|Unable to read public key for ES256 challenge ${challenge ^. ulid}: #{cryptoErr}|]
              )
        -- TODO: https://www.rfc-editor.org/rfc/rfc7518#page-9
        -- https://hackage-content.haskell.org/package/crypton-1.0.4/docs/Crypto-PubKey-ECDSA.html#t:EllipticCurveECDSA
        let Response{signature} = resp
        let rsLengthBytes = 32
        let expectedSigLength = rsLengthBytes * 2
        let sigLength = ByteString.length signature
        (r, s) <-
          if sigLength /= expectedSigLength
            then
              Left
                [i|Unable to read signature for ES256 challenge #{challenge ^. #ulid}: expected ${expectedSigLength} bytes, got ${sigLength} instead|]
            else
              pure
                ( os2ip (ByteString.take rsLengthBytes signature)
                , os2ip (ByteString.takeEnd rsLengthBytes signature)
                )
        sig <-
          ECDSA.signatureFromIntegers (Proxy @Curve_P256R1) (r, s)
            & eitherCryptoError
            & first
              ( \cryptoErr ->
                  [i|Unable to read signature for ES256 challenge ${challenge ^. ulid}: #{cryptoErr}|]
              )
        if ECDSA.verify (Proxy @Curve_P256R1) SHA256 pubKey sig (challenge ^. #nonce)
          then
            pure ()
          else
            Left [i|Invalid signature for ES256 challenge #{challenge ^. #ulid}|]
      Ed25519 -> do
        pubKey <-
          ( challenge
              ^. #pubkey
            )
            & Ed25519.publicKey
            & eitherCryptoError
            & first
              ( \cryptoErr ->
                  [i|Unable to read public key for Ed25519 challenge ${challenge ^. ulid}: #{cryptoErr}|]
              )
        sig <-
          ( resp
              ^. #signature
            )
            & Ed25519.signature
            & eitherCryptoError
            & first
              ( \cryptoErr ->
                  [i|Unable to read signature for Ed25519 challenge ${challenge ^. ulid}: #{cryptoErr}|]
              )
        if Ed25519.verify pubKey (challenge ^. #nonce) sig
          then
            pure ()
          else
            Left [i|Invalid signature for Ed25519 challenge #{challenge ^. #ulid}|]
      RS256 -> Left [i|Not implemented|]
