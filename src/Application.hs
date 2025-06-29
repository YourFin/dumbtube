{-# LANGUAGE OverloadedStrings #-}
module Application where

--import Control.Monad.Except
--import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Servant.Types.SourceT (source)
import Servant.HTML.Lucid
import Lucid

type GetHTML = Get '[HTML] (Html ())

type API = GetHTML

server1 :: Server API
server1 = pure home

api :: Proxy API
api = Proxy

appMain :: IO ()
appMain = run 8081 (serve api server1)

home :: Html ()
home = html_ $ do
  head_ $ do
    title_ "Hello!"
  body_ $ do
    h1_ "Hello, world!"
