{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# OPTIONS_GHC -fwarn-hi-shadowing #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Week5.Exercise4 where
import Prelude hiding ()
import Data.ByteString.Lazy as BS hiding (head)
import Data.ByteString.Lazy.Char8 as BS8 hiding (head)
import Servant.Client
import Servant.Docs
import Servant.API
import Options.Generic --(<?>)
import Data.Aeson
import GHC.Generics
import Data.Map as M
import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Data.Maybe


--toEncoding = genericToEncoding defaultOptions

--Example change to better fit us.
data MyJson = Config {addresses :: [String]} deriving (Generic, Show)
instance ToJSON MyJson
instance FromJSON MyJson


io = do
  file <- BS.readFile "conffi.json"
  let myJsonVal = head . addresses <$> (decode file)
      
  let
    out :: ByteString
    out = case myJsonVal of
        Just ioval -> BS8.pack ioval
        Nothing -> file
  BS8.putStrLn out
  


 -- The program should read and parse the provided JSON config files 
 --   That specify:
 --     1. which address to check. 
 --     2. a) how many times to repeat the check in case of a recoverable failure 
 --        b) and whether to present the results of the checks in color.
 
 -- The program should then issue a HEAD request to each address 
 -- and see what status code the response contains.
 --              1. If the status code indicates a recoverable failure and 
 --                 the maximum number of repetitions has not been exhausted, 
 --                 the program should try again after a short delay. 
 --              2. Otherwise, the program should stop making requests to the address.
 
 -- The program should finally collect the results and print a nicely formatted
 -- summary into the terminal.
 --               If so requested and supported by the terminal, 
 --               the summary should be decorated with bright colors.
 --
