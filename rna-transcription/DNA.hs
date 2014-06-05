module DNA (toRNA) where

toRNA :: String -> String
toRNA = map trans
  where trans 'C' = 'G'
        trans 'G' = 'C'
        trans 'A' = 'U'
        trans 'T' = 'A'
        trans _   = undefined
