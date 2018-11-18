module XMonad.Javran.Utils
  ( clamp
  , padLeftCut, padRightCut
  , dzenPutLn
  ) where

import System.IO (Handle)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (hPut)

-- | @clamp (low,high) v@ returns v if it's in range of @(low,high)@
--   otherwise the corresponding bound is returned
clamp :: (Ord a) => (a,a) -> a -> a
clamp (low,high) v
    | v < low   = low
    | v > high  = high
    | otherwise = v

-- | @padRightCut pChar l str@ or @padLeftCut pChar l str@
--   ensures a string to be of length @l@
--   by padding to left or right and then cutting down to correct length
padLeftCut, padRightCut :: a -> Int -> [a] -> [a]
padRightCut pChar l = take l . (++ repeat pChar)

padLeftCut pChar l str =
      take l
    . map snd
    . dropWhile fst
      -- use Bool to mark end of the original list
    . zip (map (const True) str ++ repeat False)
    $ cycle xs
  where
    -- begin with @cycle "AAA...."@ and remove same length
    -- as original string.
    -- by doing this we can avoid traversing the whole string.
    xs = padRightCut pChar l str


dzenPutLn :: Handle -> String -> IO ()
dzenPutLn h xs = hPut h raw
  where
    {-
      note that dzen uses utf-8 encoding, which is the first thing we need to do
      when converting to dzen-input
     -}
    raw = encodeUtf8 (pack xs)
