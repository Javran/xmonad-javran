{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , TypeApplications
  , LambdaCase
  , MultiWayIf
  #-}
module XMonad.Javran.SysInfoBar.Temperature
  ( Temperature
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Colour.Names
import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Scientific
import Data.String
import System.Directory
import System.Process
import Text.ParserCombinators.ReadP
import Text.Printf

import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified System.Dzen as Dz

import XMonad.Javran.SysInfoBar.Types

{-
  the corresponding object looks like:

  - tempX_input
  - tempX_max
  - tempX_crit

  In which X is an integer. sensors seems to simply group them by X,
  so for parsing individual ones it is fine to just ignore it.
 -}
data TempInfo
  = TempInfo
  { tiInput :: Int
  , tiMax :: Maybe Int
  , tiCrit :: Maybe Int
  } deriving (Show)

parseTempField :: T.Text -> Maybe T.Text
parseTempField inp = case readP_to_S pTemp (T.unpack inp) of
    [(v, "")] -> Just v
    _ -> Nothing
  where
    pTemp :: ReadP T.Text
    pTemp =
      T.pack <$> (
        string "temp"
        *> munch1 isDigit
        *> char '_'
        *> munch1 (const True))

instance FromJSON TempInfo where
  parseJSON = withObject "TempInfo" $ \obj -> do
    let obj' :: Object
        obj' =
          HM.fromList
          . mapMaybe (\(k,v) -> (,v) <$> parseTempField k)
          $ HM.toList obj
        cov = round @Scientific
    TempInfo
      <$> fmap cov (obj' .: "input")
      <*> (fmap . fmap) cov (obj' .:? "max")
      <*> (fmap . fmap) cov (obj' .:? "crit")

{-
  naming here is a bit confusing, the following doc looks authoritative on this topic:

  https://www.kernel.org/doc/Documentation/hwmon/sysfs-interface
 -}

type TempInfoTable = M.Map T.Text [TempInfo] -- this list is guaranteed to be non-empty

data Criticality = CNormal | CHigh | CCritical

type TempDisplay = (Int, Criticality)

readFromSensors :: String -> IO (Maybe TempDisplay, Maybe TempDisplay)
readFromSensors binPath = do
  let cp =
        (shell $ unwords [binPath, "-j", "-A"])
          { std_in = NoStream
          , std_out = CreatePipe
          , std_err = Inherit
          }
  (_, Just hOut, _, _ph) <- createProcess cp
  raw <- BSL.hGetContents hOut
  -- somehow not waiting for process does what we want this to do...
  case eitherDecode' @(M.Map T.Text (M.Map T.Text (Maybe TempInfo))) raw of
    Left _e -> pure (Nothing, Nothing)
    Right parsed -> do
      let tbl :: TempInfoTable
          tbl =
            -- non-empty list only, with those that can be parsed successfully.
            M.filter (not . null)
            . M.map (catMaybes . M.elems)
            $ parsed
          toDisplay :: T.Text -> Maybe TempDisplay
          toDisplay prop = do
            vs <- tbl M.!? prop
            let ti = maximumBy (comparing tiInput) vs
                inp = tiInput ti
                Just crit =
                    shouldShowCrit
                    <|> shouldShowHigh
                    <|> Just CNormal
                  where
                    shouldShowCrit = do
                      critBound <- tiCrit ti
                      guard $ inp >= critBound
                      pure CCritical
                    shouldShowHigh = do
                      highBound <- tiMax ti
                      guard $ inp >= highBound
                      pure CHigh
            pure (inp, crit)
      pure (toDisplay "coretemp-isa-0000", toDisplay "acpitz-acpi-0")


data Temperature

renderDzen :: Maybe TempDisplay -> Dz.DString
renderDzen = \case
  Nothing -> "??" -- no reading, treat it as normal.
  Just (inp, crit) ->
    let numDisplay =
          if
            | inp < 0 -> "NE"
            | inp >= 100 -> "!!"
            | otherwise -> fromString (printf "%2d" inp)
        sty = case crit of
          CNormal -> id
          CCritical -> Dz.fg red
          CHigh -> Dz.fg orange
    in sty numDisplay

instance Worker Temperature where
  workerStart _ sendMessage = do
    {-
      Query for the binary once and lock on the decision.
      Most of the time if we cannot find the binary first time,
      there's no further need to repeat this process over and over again.
      User can simply restart XMonad to redo the scan.
     -}
    mSensorsBinPath <- findExecutable "sensors"
    case mSensorsBinPath of
      Nothing -> forever $ do
        sendMessage (MPRendered Nothing)
        threadDelay $ 4 * 1000 * 1000
      Just binPath -> forever $ do
        (cpuTemp, acpiTemp) <- readFromSensors binPath
        let rendered = renderDzen cpuTemp <> " " <> renderDzen acpiTemp
        sendMessage (MPRendered (Just rendered))
        threadDelay $ 500 * 1000

  workerDeadline _ = 5
