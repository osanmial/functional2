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
{-# LANGUAGE ScopedTypeVariables #-}


module Week5.Exercise4 where
import Network.HTTP.Client.TLS  hiding (Proxy)
import Prelude hiding (readFile)
import Data.ByteString.Lazy as BS hiding (head, putStrLn, putStr, repeat, take, length)
import Network.HTTP.Types.Status (statusCode)
import Options.Generic
import Data.Aeson
import Network.HTTP.Client hiding (Proxy)
import System.Console.ANSI
import Control.Exception


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
  MyJson addresses <- readJSON "src/Week5/sites.json"
  let iolines = fmap handleLine addresses
  lineList <- sequenceA iolines
  printSites lineList
  

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
connectUrl = f 1  
  where
    f :: Int -> String -> Int -> IO (Int, Int)
    f curAttempt siteUrl maxAttempts = do
      res <- requestUrl siteUrl

      let final
            | res == 200                = pure (res, curAttempt)
            | curAttempt >= maxAttempts = pure (res, curAttempt)
            | otherwise                 = f (succ curAttempt) siteUrl maxAttempts

      final
        
      
      


requestUrl :: String -> IO Int
requestUrl siteUrl = catch f
                     (\((HttpExceptionRequest _ c) :: HttpException) -> return 666)
  where
    f :: IO Int
    f = do
      manager <- newTlsManagerWith tlsManagerSettings
      prerequest <- parseRequest $ siteUrl
      let request = prerequest
                    { method = "HEAD",
                      responseTimeout = responseTimeoutMicro (1000 * 1000) }
      response <- httpLbs request manager
      return (statusCode $ responseStatus response)


data Site = Site {
  siteName :: String,
  siteResponse :: Int,
  siteAttempts :: Int,
  totalTries:: Int,
  colors :: Bool
  } deriving Show

printSites :: [Site] -> IO ()
printSites sites  = f (maxNameLen sites) sites
  where
    f _ []     = pure ()
    f l (x:xs) = printSite l x >> f l xs
    maxNameLen []     = 0
    maxNameLen (x:xs) = max (length (siteName x)) (maxNameLen xs)

printSite :: Int -> Site -> IO ()
printSite maxLen site = do

  putStr $ take  (maxLen - (length (siteName site))) (repeat ' ')

  if colors site then do  
    setSGR [ SetColor Foreground Vivid White ]
    putStr $ siteName site
    putStr " "
    setSGR [ SetColor Foreground Vivid Green ]
    putStr $ show $ siteResponse site
    putStr " "
    setSGR [ SetColor Background Dull Blue ]
    putStr $ take (siteAttempts site) (repeat ' ')
    setSGR [ SetColor Background Dull White ]
    putStr $ take ((totalTries site) - (siteAttempts site)) (repeat ' ')
    setSGR [ Reset ]
    putStrLn ""
    else do
    putStr $ siteName site
    putStr " "
    putStr $ show $ siteResponse site
    putStr " "
    putStr $ take (siteAttempts site) (repeat '=')
    putStrLn $ take ((totalTries site) - (siteAttempts site)) (repeat '-')
  

