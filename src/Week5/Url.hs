{-#LANGUAGE OverloadedStrings#-}
module Url  where 
import Data.Aeson
import Data.ByteString
Data.Text.Internal.Text

data Url =
  Url { url  :: !Text
         , numberOfRepeats :: Int
         , useColor :: Bool
           } deriving Show


instance FromJSON Url where
 parseJSON (Object v) =
    Url <$> v .: "url"
           <*> v .: "numberOfRepeats"
           <*> v .: "useColor"
 parseJSON _ = mzero

instance ToJSON Url where
 toJSON (Url  url numberOfRepeats useColor) =
    object [ "url"  .= url
           , "numberOfRepeats".= numberOfRepeats
           , "useColor"        .= useColor           
             ]