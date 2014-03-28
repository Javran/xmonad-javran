import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.List
import System.IO
import System.Environment (getArgs)
import Text.JSON.String
import Text.JSON.Types

import qualified Data.Map as M

import JavranXMonad.Utils

-- InfoRaw <slot> <tag> <data>
data InfoRaw = InfoRaw String (Maybe String) String
    deriving Show

-- Info <slot> <tag> <converted data>
data Info = Info String (Maybe String) String
    deriving Show

-- convert json raw string, return a formatted string according to the template
convertJson :: String -> String -> String
convertJson template rawJson = either (const rawJson) (doConvert template rawJson)
    $ runGetJSON readJSTopType rawJson

-- try to convert the Json parsing result
doConvert :: String -> String -> JSValue -> String
doConvert template fallbackStr parsedJson = maybe
    -- on failure
    fallbackStr
    -- on success
    (applyTemplate template Nothing . map (applySlot convertSlots)) 
    $ maybeConvert parsedJson
    where
        maybeConvert :: JSValue -> Maybe [InfoRaw]
        maybeConvert obj = liftM (map pairToInfoRaw) $ maybeConvertObject obj
            where pairToInfoRaw (k,v) = InfoRaw s t v
                    where (s,t) = pairSlotTag k

        maybeConvertObject :: JSValue -> Maybe [(String, String)]
        maybeConvertObject (JSObject obj) = 
            Just $ mapMaybe maybeConvertPair $ fromJSObject obj
            where
                maybeConvertPair (k,v) = do
                    vStr <- maybeConvertString v
                    return (k,vStr)
        maybeConvertObject _ = Nothing

        maybeConvertString :: JSValue -> Maybe String
        maybeConvertString (JSString s) = Just $ fromJSString s
        maybeConvertString _ = Nothing

pairSlotTag :: String -> (String, Maybe String)
pairSlotTag str = (slot, tag)
    where
        (slot,uTag) = span (/='_') str
        tag = if length uTag <= 1
            then Nothing
            else Just $ tail uTag

applySlot :: M.Map String (String -> String) -> InfoRaw -> Info
applySlot slots (InfoRaw s t d) = Info s t $ handler d
    where
        handler = fromMaybe id $ M.lookup s slots

-- make sure `str` has length of exactly `len`,
--   if `str` is too short, use `padChar` to pad
--   if `str` is too long , use `fallbackStr` instead
fixStringLen :: Int     -- length expected
             -> Char    -- too short
             -> String  -- too long
             -> String  -- input
             -> String  -- output
fixStringLen len padChar fallbackStr str =
    keepStringLength 
        len
        (padLeft (len-strLen) padChar)
        (const fallbackStr)
        str
    where strLen = length str

-- pretty print bit count
bitToReadableString :: Int -> String
bitToReadableString b
    | b < unitKiB && b       < 1000 = fixLen $ show b       ++   "B"
    | b < unitKiB                   =                       "0.9KiB"
    | b < unitMiB && bDivKiB < 1000 = fixLen $ show bDivKiB ++ "KiB"
    | b < unitMiB                   =                       "0.9MiB"
    | b < unitGiB && bDivMiB < 1000 = fixLen $ show bDivMiB ++ "MiB"
    | b < unitGiB                   =                       "0.9GiB"
    | otherwise                     = fixLen                ">=1GiB"
    where
        fixLen = fixStringLen 6 ' ' undefined
        unitKiB = 1024
        unitMiB = 1024 *  unitKiB
        unitGiB = 1024 *  unitMiB
        bDivKiB = b `div` unitKiB
        bDivMiB = b `div` unitMiB

-- a mapping from slot name to the corresponding handler
convertSlots :: M.Map String (String -> String)
convertSlots = M.fromList
    [ ("date"       , id                )
    , ("time"       , id                )
    , ("mem"        , convertMemLoad    )
    , ("cpu"        , convertCpuLoad    )
    , ("netspeed"   , convertNetspeed   )
    , ("adapter"    , convertAdapter    )
    , ("battery"    , convertBattery    )
    , ("mpdstatus"  , convertMpdStatus  )
    , ("top"        , convertTop        )
    , ("freq"       , convertFreq       )
    , ("mail"       , convertMailCheck  )
    ]
    where
        convertCpuLoad s = fullOrNum $ keepInRange (0,100) $ read s
            -- the padChar should never be reached here
            where fullOrNum x = fixStringLen 1 undefined "F" $ show $ x `div` 10
        convertMemLoad s = fixStringLen 2 ' ' "FF" s ++ "%"
        convertNetspeed s = bitToReadableString speedB
            where
                speedKiB = sum $ map read $ words s :: Float
                speedB = floor $ 1024 * speedKiB
        convertAdapter "on-line" = "+"
        convertAdapter _         = "="

        convertBattery s = fixStringLen 4 ' ' undefined $ s ++ "%"

        convertMpdStatus "Playing" = ">"
        convertMpdStatus "Paused"  = "|"
        convertMpdStatus "Stopped" = "-"
        convertMpdStatus _         = "?"

        -- "AAAA.." max len = 6
        convertTop s
            | len > limit = take 4 s' ++ ".."
            | otherwise   = s' ++ replicate (limit-len) ' '
            where
                s' = fromMaybe "???" $ listToMaybe $ words s
                len = length s'
                limit = 6

        convertFreq = (++ "GHz")

        convertMailCheck xs
            | null xs = "?"
            | length xs > 1 = "+"
            | otherwise = xs

-- '!' for escaping
-- "{..}" -> tag
-- "[..]" -> color
applyTemplate :: String -> Maybe String -> [Info] -> String
applyTemplate [] colorEnd _ = "" ++ fromMaybe "" colorEnd
applyTemplate (t:ts) colorEnd infos =
    case t of
        '!' -> case ts of
        -- these cases are actually the same...
            []       -> '!' : applyTemplate ts colorEnd infos 
            (t':ts') ->  t' : applyTemplate ts' colorEnd infos 
        '[' -> colorStr ++ applyTemplate restStr newColorEnd infos 
            where
                (colorStr,restStr, newColorEnd) = doColor ts
                doColor str
                    | null rest        = ("[", ts, colorEnd)
                    | head rest == '[' = ("[", ts, colorEnd)
                    | null body        = ("[]", tail rest, colorEnd)
                    | otherwise        = (prevColorEnd ++ "^fg("++body++")", tail rest, Just "^fg()")
                    where
                        (body,rest) = span (`notElem` "[]") str
                        prevColorEnd = fromMaybe "" colorEnd
        '{' -> formattedStr ++ applyTemplate restStr colorEnd infos 
            where
                (formattedStr, restStr) = doFormat ts
                doFormat str
                    | null rest        = ("{", ts)
                    | head rest == '{' = ("{", ts)
                    | null body        = ("[]", tail rest)
                    | otherwise        = (findData body infos, tail rest)
                    where
                        (body,rest) = span (`notElem` "{}") str
        _ -> t : applyTemplate ts colorEnd infos 
        where
            findData :: String -> [Info] -> String
            findData body infos
                | isNothing tag = maybe fallback getData $ find (matchSlot slot) infos
                | otherwise     = maybe fallback getData $ find (matchTag slot tag) infos
                where
                    (slot,tag) = pairSlotTag body
                    matchSlot slot (Info s _ _) = s == slot
                    matchTag slot tag (Info s t _) = s == slot && t == tag
                    getData  (Info _ _ d) = d
                    fallback = "{" ++ body ++"}"

main :: IO ()
main = do
    -- should have one argument
    args <- getArgs
    case args of
        []        -> showHelp
        (fname:_) -> readTemplate fname 
    where
        readTemplate = runKleisli
            $   Kleisli readFile 
            >>> arr lines
            >>> arr concat
            >>> Kleisli convertLine
        
-- print help message
showHelp :: IO ()
showHelp = putStrLn "StreamConvert <template file>"

-- convert every input line until EOF
convertLine :: String -> IO ()
convertLine template = do
    r <- isEOF
    unless r $ 
            getLine 
        >>= putStrLn . convertJson template
        >>  hFlush stdout
        >>  convertLine template
