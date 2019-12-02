{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# OPTIONS_GHC -fwarn-hi-shadowing #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Week5.Exercise4 where
import Network.HTTP.Client.TLS  hiding (Proxy)
--import Network.HTTP.Simple hiding (Proxy)
import Prelude as P
import Data.ByteString.Lazy as BS hiding (head)
import Data.ByteString.Lazy.Char8 as BS8 hiding (head)
import Servant.Client hiding (responseBody)
import Network.HTTP.Types.Status (statusCode)
--import Servant.Docs
import Servant.API
import Options.Generic --(<?>)
import Data.Aeson
import GHC.Generics
import Data.Map as M
import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Data.Maybe

import System.Console.ANSI


--toEncoding = genericToEncoding defaultOptions

--Example change to better fit us.
data MyJson = Config {addresses :: [String]} deriving (Generic, Show)
instance ToJSON MyJson
instance FromJSON MyJson




myReadFile :: IO ByteString
myReadFile = do
  file <- BS.readFile "conffi.json"
  let myJsonVal = head . addresses <$> (decode file)
  let
    out :: ByteString
    out = case myJsonVal of
        Just ioval -> BS8.pack ioval
        Nothing -> file
  pure out

data IP = IP {ip :: String} deriving (Generic, Show)

--type Addr = String deriving (Generic, Show)

instance ToJSON IP
instance FromJSON IP

data Empty a = Empty a deriving (Generic, Show)
instance ToJSON a => ToJSON (Empty a)
instance FromJSON a => FromJSON (Empty a)


type Head contentTypes a = Verb 'HEAD 200 contentTypes a
type API = Get '[JSON] IP --Get '[JSON] IP --Works
api :: Proxy API
api = Proxy


api' = client api

--getHead :: ClientM ()
getHead = do
  out <- api'
  pure out

--headHTTP :: String -> IO (Either String String)
--headHTTP :: String -> IO ()
headHTTP hostName = do
  let bUrl = BaseUrl Http hostName 80 ""  -- parseBaseUrl hostName
  manager' <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager' bUrl
  res <- runClientM (getHead) clientEnv
  case res of
    Left err     -> P.putStrLn $ "Error " ++ show err
    Right alpaca -> P.putStrLn $ "output "++  show alpaca



requ :: String -> IO ()
requ url = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest $ "http://" ++ url
  let inittedReq = request {method = "HEAD"}
  response <- httpLbs request manager

  P.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response


iotest :: IO ()
iotest = do
  host <- myReadFile
  requ (BS8.unpack host)


duck = Site "www.duckduckgo.com" 200 1 5 True

data Site = Site {
  siteName :: String,
  response :: Int,
  attempts :: Int,
  totalTries:: Int,
  colors :: Bool
  } deriving Show


printSite (Site name response attempts totalTries color) = do

  if color then do
    setSGR [ SetColor Foreground Vivid White ]
    Prelude.putStr name
    Prelude.putStr " "
    setSGR [ SetColor Foreground Dull White ]
    Prelude.putStr (show response)
    Prelude.putStr " "
    setSGR [ SetColor Background Dull Blue ]
    Prelude.putStr (Prelude.take attempts (Prelude.repeat ' '))
    setSGR [ SetColor Background Dull White ]
    Prelude.putStr (Prelude.take (totalTries - attempts) (Prelude.repeat ' '))
    setSGR [ Reset ]
    Prelude.putStrLn ""
    else do
    Prelude.putStr name
    Prelude.putStr " "
    Prelude.putStr (show response)
    Prelude.putStr " "
    Prelude.putStr (Prelude.take attempts (Prelude.repeat '='))
    Prelude.putStrLn (Prelude.take (totalTries - attempts) (Prelude.repeat '-'))
  

