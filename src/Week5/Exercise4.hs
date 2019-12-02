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
import Prelude hiding (readFile)
import Data.ByteString.Lazy as BS hiding (head, putStrLn, putStr, repeat, take)
--import Data.ByteString.Lazy.Char8 as BS8 hiding (head)
--import Servant.Client
import Network.HTTP.Types.Status (statusCode)
--import Servant.Docs
--import Servant.API
import Options.Generic --(<?>)
import Data.Aeson
--import GHC.Generics
--import Data.Map as M
--import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
--import Data.Maybe

import System.Console.ANSI


data MyJson = MyJson [AddressLine] deriving (Generic, Show)

instance ToJSON MyJson
instance FromJSON MyJson


data AddressLine = AddressLine {
  url :: String,
  numberOfRepeats :: Int,
  useColor :: Bool
  } deriving (Generic, Show)

instance ToJSON AddressLine
instance FromJSON AddressLine


dothething = do
  MyJson addresses <- readJSON "test.json"
  let iolines = fmap handleLine addresses
  lines <- sequenceA iolines
  printSites lines
  

readJSON :: FilePath -> IO MyJson
readJSON fileName = do
  file <- readFile fileName
  case decode file :: Maybe MyJson of
    Just x  -> return x
    Nothing -> return (MyJson [])


handleLine :: AddressLine -> IO Site
handleLine line = do
  (res, att) <- connectUrl (url line) (numberOfRepeats line)
  return $ Site (url line) res att (numberOfRepeats line) (useColor line)

connectUrl :: String -> Int -> IO (Int, Int)
connectUrl url maxAttempts = do
  res <- requestUrl url
  return (res, 1)


requestUrl :: String -> IO Int
requestUrl url = do
  manager <- newTlsManagerWith tlsManagerSettings

  request <- parseRequest $ url
  let inittedReq = request {method = "HEAD"}
  response <- httpLbs inittedReq manager

  return (statusCode $ responseStatus response)



duck = Site "www.duckduckgo.com" 200 1 5 True

data Site = Site {
  siteName :: String,
  siteResponse :: Int,
  siteAttempts :: Int,
  totalTries:: Int,
  colors :: Bool
  } deriving Show

printSites :: [Site] -> IO ()
printSites []     = pure ()
printSites (x:xs) = printSite x >> printSites xs

printSite :: Site -> IO ()
printSite (Site name response attempts totalTries color) = do

  if color then do
    setSGR [ SetColor Foreground Vivid White ]
    putStr name
    putStr " "
    setSGR [ SetColor Foreground Dull White ]
    putStr (show response)
    putStr " "
    setSGR [ SetColor Background Dull Blue ]
    putStr (take attempts (repeat ' '))
    setSGR [ SetColor Background Dull White ]
    putStr (take (totalTries - attempts) (repeat ' '))
    setSGR [ Reset ]
    putStrLn ""
    else do
    putStr name
    putStr " "
    putStr (show response)
    putStr " "
    putStr (take attempts (repeat '='))
    putStrLn (take (totalTries - attempts) (repeat '-'))
  

