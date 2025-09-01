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
import Effectful hiding ((:>))
import Effectful qualified as Effectful
import Effectful.Error.Static qualified as Error
import Effectful.Fail (Fail, runFail)
import Effectful.Resource (Resource)
import Effectful.Resource qualified as Resource
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.HTTP.Types.Header qualified as Header
import Network.Wai
import Network.Wai.Handler.Warp
import Optics ((.~))
import Servant
import Servant.HTML.Lucid
import Servant.Server qualified as ServantServer
import Servant.Types.SourceT (source)
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Directory
import Text.RawString.QQ (r)
import Yf.Effect.Resource.Pool (maxResidency)
import Yf.Effect.Resource.Pool qualified as Pool
import Yf.Effect.Servant
import Yf.Effect.Sqlite
import Yf.Effect.Sqlite qualified as Sqlite
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

server1 ::
  ((Effectful.:>) Resource es, (Effectful.:>) ErrorPage es, (Effectful.:>) Sqlite es) =>
  ServerT API (Eff es)
server1 =
  failToErrorPage
    ( do
        Sqlite.execute_
          [r|
    UPDATE Data
    SET value = ((SELECT value FROM Data WHERE key = 'visitor_num') + 1);
    |]
        [[viewCount :: Int]] <-
          Sqlite.query_ [r|SELECT value FROM Data where key = 'visitor_num';|] S.toList_
        pure $ home viewCount
    )

failToErrorPage ::
  (HasCallStack, (Effectful.:>) ErrorPage es) => Eff (Fail : es) a -> Eff es a
failToErrorPage action = do
  result <- runFail action
  case result of
    Left ex ->
      Error.throwError_ (errorPage ex)
    Right val ->
      pure val
 where
  errorPage :: String -> ServerError
  errorPage ex =
    ServantServer.ServerError
      500
      "Internal Server Error"
      (renderBS $ body ex)
      [(Header.hContentType, "text/html; charset=UTF-8")]

  body :: String -> Html ()
  body ex = html_ $ do
    head_ $ do
      title_ "Hello!"
    body_ $ do
      h1_ "Hello, world!"
      fromString ex

api :: Proxy API
api = Proxy

appMain :: IO ()
appMain = runMain $ do
  Sqlite.execute_
    "CREATE TABLE Data (key TEXT PRIMARY KEY, value BLOB);"
  Sqlite.execute_ "INSERT INTO Data (key, value) VALUES ('visitor_num', 0);"
  runTLS
    @API
    (tlsSettings "/home/pen/.local/yf/cert.pem" "/home/pen/.local/yf/key.pem")
    8443
    server1

runMain :: Eff '[Sqlite, Resource, IOE] a -> IO a
runMain action =
  action
    & Sqlite.runPool
      ( Sqlite.pool "file::memory:?cache=shared"
          & Pool.maxResidency (Pool.systemThreads * Pool.systemThreads)
          & (#max .~ 4)
          & Pool.stripes 1
      )
    & Resource.runResource
    & runEff

-- appMain = do
--  ctx <- Tpm.initialize
--  randomData <- Tpm.random ctx 3
--  print $ show randomData

home :: Int -> Html ()
home viewCount = html_ $ do
  head_ $ do
    title_ "Hello!"
  body_ $ do
    h1_ "Hello, world!"
    "You are visitor number: " <> show viewCount
    button_ [id_ "creds"] "Create creds"
    p_ [id_ "results"] ""

  ( script_
      [r|
document.querySelector('#creds').addEventListener("click", async (e) => {
e.preventDefault();
let credsResults = await navigator.credentials.create({
  publicKey: {
    rp: { name: "localhost:3000", },
    user: {
      id: new Uint8Array([79, 252, 83, 72, 214, 7, 89, 26]),
      displayName: "Pen",
      name: "pen@localhost",
    },
    challenge: new Uint8Array([79, 252, 83, 72, 214, 7, 89, 26]),
    authenticatorSelection: {
      residentKey: "required",
      requireResidentKey: true,
      userVerification: "discouraged",
    },
    pubKeyCredParams: [
      { type: "public-key",
        alg: -7, // IANA COSE Algorithms registry id for alg
      },
      { type: "public-key",
        alg: -257,
      },
    ],
    hints: [ "client-device" ],
  },
});
let result = document.getElementById("results");
document.result = result;
result.textContent = JSON.stringify(result);
});
|]
    )
