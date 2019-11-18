module Week3.Exercise4 where

import Control.Applicative
import Week3.Exercise3

 
record = (\x -> \y -> x:y) <$> unit <*> recordTail
recordTail = (many ((flip const) <$> unitSeparator <*> unit))
recordSeparator = single '\n' 

unit = unitValueUnquoted <|> unitValueQuoted
unitValueUnquoted = some (noneOf [',', '\n', '\r', '"'])
unitValueQuoted = flip const <$> single '"'
                  <*> (const <$> some (anySingleBut '"')
                       <*> single '"')
--doesn't work
insideQuotes = fmap concat (some ((chunk "\"\"") <|> fmap (\x -> [x]) (anySingleBut '"')))

unitSeparator = single ','

