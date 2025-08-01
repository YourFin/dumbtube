{-# LANGUAGE OverloadedStrings #-}

module Application where

-- import Control.Monad.Except
-- import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Servant.Types.SourceT (source)
import System.Directory
import Yf.Tpm qualified as Tpm

type GetHTML = Get '[HTML] (Html ())

type API = GetHTML

{-
Works two ways:
1.
  Server ~somehow~ determines client identity
  Server sends client id forward, along with challenge
  Client decrypts client id into private key, signs challenge
  Server verifies challenge
2.
  Client visits page
  Page looks up authn stored in browser
  Client signs response

{
  rp: { // relying party
    id: <origin> i.e. https://5z.vc
    name: "5z.vc"
  },
  user: {
    id: <64 bytes>, // PK, random, sever side
    displayName: string, // user decides, at least 64 bytes
                         // Nickname profile enforcement?
    name: string, // i.e. email address
  },
  challenge: <bytes>, // random? crypto thing
  pubKeyCredParams: [
    { type: "public-key",
      alg: <long>, // IANA COSE Algorithms registry id for alg
    }
  ],
  timeout: <milliseconds>
  authenticatorSelection: {
    authenticatorAttachment?:
      "platform", // i.e. TPM, part of device
    | "cross-platform", // i.e. mobile phone with ble, ubikey
    // "touch the fingerprint reader"
    userVerification:
      "preferred" <default> //
    | "required"
    | "discouraged"
  } &
    // MUST be able to map origin -> identity on client
    ( { residentKey: "required", requireResidentKey: true }
    // Server may need to ship key forward via ID
    | { residentKey: "discouraged", reqireResidentKey: false (implicit) }
    | { residentKey: "preferred" }
    )
  },
  // public key
  excludeCredentials: [{ type: "public-key", id: <id> }],

}
-}

server1 :: Server API
server1 = pure home

api :: Proxy API
api = Proxy

appMain :: IO ()
-- appMain = run 8081 (serve api server1)
appMain = do
  ctx <- Tpm.initialize
  randomData <- Tpm.random ctx 3
  print $ show randomData

home :: Html ()
home = html_ $ do
  head_ $ do
    title_ "Hello!"
  body_ $ do
    h1_ "Hello, world!"
