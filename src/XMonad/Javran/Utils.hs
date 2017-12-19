module XMonad.Javran.Utils
  ( clamp
  , padLeft, padRight,
  ) where

-- | @clamp (low,high) v@ returns v if it's in range of @(low,high)@
--   otherwise the corresponding bound is returned
clamp :: (Ord a) => (a,a) -> a -> a
clamp (low,high) v
    | v < low   = low
    | v > high  = high
    | otherwise = v

-- | pad some chars to the string
padLeft, padRight :: Int -> Char -> String -> String
padLeft  padLen padChar input = replicate padLen padChar ++ input
padRight padLen padChar input = input ++ replicate padLen padChar
