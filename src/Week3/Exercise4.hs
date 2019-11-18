module Week3.Exercise4 where

import Week3.Exercise3


unitValue = (\x -> [x]) <$> noneOf [',']

separator = single ','



-- chunk :: String -> Parser String
-- chunk []     = fmap (\x -> "") eof
-- chunk (x:xs) = pure (++) <*> (fmap (\x -> [x]) (single x)) <*> chunk xs
