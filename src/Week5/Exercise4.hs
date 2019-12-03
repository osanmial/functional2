{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# OPTIONS_GHC -fwarn-hi-shadowing #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}


--TODO:
-- The current solution only tries once, in connectUrl
-- Handle errors, in requestUrl
-- Even out the lines in printing



module Week5.Exercise4 where
import Network.HTTP.Client.TLS  hiding (Proxy)
import Prelude hiding (readFile)
import Data.ByteString.Lazy as BS hiding (head, putStrLn, putStr, repeat, take)
import Network.HTTP.Types.Status (statusCode)
import Options.Generic
import Data.Aeson
import Network.HTTP.Client hiding (Proxy)
import System.Console.ANSI

--coerce

newtype MyJson = MyJson [AddressLine] deriving (Generic, Show)

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
  (res, att) <- connectUrl (url line) 1 (numberOfRepeats line)
  return $ Site (url line) res att (numberOfRepeats line) (useColor line)

connectUrl :: String -> Int -> Int -> IO (Int, Int)
connectUrl url curAttempt maxAttempts = do
  res <- requestUrl url
  return (res, curAttempt)


requestUrl :: String -> IO Int
requestUrl url = do
  manager <- newTlsManagerWith tlsManagerSettings

  request <- parseRequest $ url
  let inittedReq = request {method = "HEAD"}
  response <- httpLbs inittedReq manager

  return (statusCode $ responseStatus response)


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
    setSGR [ SetColor Foreground Vivid Green ]
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
  

