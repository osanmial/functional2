module Week3.Exercise4 where

import Control.Applicative
import Week3.Exercise3

group = (:) <$> record <*> ((++) <$> groupTail <*> (fmap (\x -> []) eof))
groupTail = many ((flip const) <$> recordSeparator <*> record)

record = (:) <$> unit <*> recordTail
recordTail = (many ((flip const) <$> unitSeparator <*> unit))
recordSeparator = fmap (\x -> [x]) (single '\n') <|> chunk2 "\r\n"

unit = unitValueUnquoted <|> unitValueQuoted <|> pure ""
unitValueUnquoted = some (noneOf [',', '\n', '\r', '"'])
unitValueQuoted = flip const <$> single '"'
                  <*> (const <$> insideQuotes
                       <*> single '"')
insideQuotes = fmap concat (some ((chunk2 "\"\"") <|> fmap (\x -> [x]) (anySingleBut '"')))
unitSeparator = single ','


testcase = "#0,#1,#2,#3,#4,#5,#6,#7,#8,#9\n\
           \0,1,2,3,4,5,6,7,8\n\
           \1,1,1,1,one as a string,1,1,1,1\n\
           \2,1,2,1,2,1,2,1,2\n\
           \3,1,1,3,\"one, but with a comma\",1,3,1,1\n\
           \4,1,2,1,4,1,2,1,4\n\
           \5,1,1,1,\"one accompanied by\n\
           \a line break\",5,1,1,1\n\
           \6,1,2,3,2,1,6,1,2\n\
           \7,1,1,1,\"one involving \"\"quotation\"\" marks\",1,1,7,1\n\
           \8,1,2,1,4,1,2,1\n\
           \,,,,\"one among missing values\",,,,,9"
