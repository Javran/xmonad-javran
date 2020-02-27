module XMonad.Javran.Utils
  ( clamp
  , padLeftCut, padRightCut
  , dzenPutLn
  , fixStringLen
  , byteToReadableString
  , appendLogTo
  ) where

import System.IO (Handle)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (hPut)
import Data.Time

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

-- make sure `str` has length of exactly `len`,
--   if `str` is too short, use `padChar` to pad
--   if `str` is too long , use `fallbackStr` instead
fixStringLen :: Int     -- length expected
             -> Char    -- too short
             -> String  -- too long
             -> String  -- input
             -> String  -- output
fixStringLen len padChar fallbackStr str =
    case strLen `compare` len of
        LT -> padLeftCut padChar len str
        EQ -> str
        GT -> fallbackStr
    where
      strLen = length str

-- pretty print byte count
byteToReadableString :: Int -> String
byteToReadableString b
    | b < unitKiB && b       < 1000 = fixLen $ show b       ++   "B"
    | b < unitKiB                   =                       "0.9KiB"
    | b < unitMiB && bDivKiB < 1000 = fixLen $ show bDivKiB ++ "KiB"
    | b < unitMiB                   =                       "0.9MiB"
    | b < unitGiB && bDivMiB < 1000 = fixLen $ show bDivMiB ++ "MiB"
    | b < unitGiB                   =                       "0.9GiB"
    | otherwise                     = fixLen                ">=1GiB"
    where
        fixLen = fixStringLen 6 ' ' "??????"
        unitKiB = 1024
        unitMiB = 1024 *  unitKiB
        unitGiB = 1024 *  unitMiB
        bDivKiB = b `div` unitKiB
        bDivMiB = b `div` unitMiB

appendLogTo :: FilePath -> String -> IO ()
appendLogTo logPath msg = do
  t <- getZonedTime
  let dateStr = formatTime defaultTimeLocale "%_Y-%m-%d" t
      timeStr = formatTime defaultTimeLocale "%T" t
      header = "[" <> dateStr <> " " <> timeStr <> "]"
  appendFile logPath (header <> " " <> msg <> "\n")
