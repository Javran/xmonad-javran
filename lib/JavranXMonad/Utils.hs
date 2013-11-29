module JavranXMonad.Utils
( keepInRange
, keepStringLength
, padLeft
, padRight
) where

-- make sure the result is within a range
keepInRange :: (Ord a) => (a,a) -> a -> a
keepInRange (low,high) v
    | v < low   = low
    | v > high  = high
    | otherwise = v

-- make sure the resulting string is a fixed length
keepStringLength :: Int                 -- fixed length
                 -> (String -> String)  -- what if it's too long
                 -> (String -> String)  -- what if it's too short
                 -> String              -- input
                 -> String              -- output
keepStringLength len tooShortProc tooLongProc input
    | strLen >  len = tooLongProc  input
    | strLen == len = input
    | otherwise     = tooShortProc input
    where
        strLen = length input

-- pad some chars to the string
padLeft, padRight :: Int -> Char -> String -> String
padLeft  padLen padChar input = replicate padLen padChar ++ input
padRight padLen padChar input = input ++ replicate padLen padChar
