{-# LANGUAGE
    ExplicitForAll
  , LambdaCase
  , ScopedTypeVariables
  , OverloadedStrings
  , TypeOperators
  , TypeFamilies
  , TypeApplications
  , MultiWayIf
  #-}
module XMonad.Javran.SysInfoBar.DzenRender where

import System.Dzen
import Data.Proxy
import Data.String
import Data.Typeable
import Data.Monoid
import Data.Colour.Names
import Data.Maybe
import Text.Printf
import Control.Lens ((<&>))
import Data.Colour.SRGB
import System.Dzen.Internal (primStr)
import qualified Network.MPD as Mpd

import XMonad.Javran.Utils
import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.SysInfoBar.CpuUsage (CpuUsage)
import XMonad.Javran.SysInfoBar.CpuMaxFreq (CpuMaxFreq)
import XMonad.Javran.SysInfoBar.MemUsage (MemUsage)
import XMonad.Javran.SysInfoBar.TopProc (TopProc)
import XMonad.Javran.SysInfoBar.NetStat (NetStat)
import XMonad.Javran.SysInfoBar.Mail (Mail)
import XMonad.Javran.SysInfoBar.Mpd (Mpd)
import XMonad.Javran.SysInfoBar.Battery (Battery)
import XMonad.Javran.SysInfoBar.DateTime (DateTime)

renderCpuUsage :: [Int] -> DString
renderCpuUsage xs = "[" <> foldMap rdr xs <> "]"
  where
    rdr :: Int -> DString
    rdr n
      | n >= 9 = fg red (if n > 9 then "X" else "9")
      | n >= 5 = fg orange (fromString (show n))
      | otherwise = fromString (show n)

renderCpuMaxFreq :: Maybe Double -> DString
renderCpuMaxFreq = \case
    Nothing -> "????GHz"
    Just d ->
      let content :: String
          content = printf "%4.2fGHz" d
      in fromString content

renderMemUsage :: (Int, Int) -> DString
renderMemUsage (numer, denom) = fromString ("M:" ++ msg)
  where
    fI = fromIntegral @_ @Double
    msg :: String
    msg = if
        | numer > denom -> "ERR"
        | numer == denom -> "##%" -- fully occupied
        | numer < 0 -> "ERR"
        | otherwise ->
            let pc = fI numer * 100 / fI denom
            in printf "%2d%%" (floor pc :: Int)

renderTopProc :: Maybe String -> DString
renderTopProc = \case
    Nothing -> "------"
    Just xs | length xs <= 6 -> fromString $ take 6 (xs ++ repeat ' ')
    Just xs -> fromString $ take 4 xs ++ ".."

renderNetStat :: (Int, Int) -> DString
renderNetStat (rBytes, tBytes) =
    rContent <> " " <> tContent
  where
    rContent =
        fg (sRGB24read "#80FF80")
      . fromString
      $ "R:" ++ byteToReadableString rBytes
    tContent =
        fg (sRGB24read "#8080FF")
      . fromString
      $ "T:" ++ byteToReadableString tBytes

renderMail :: Maybe Int -> DString
renderMail mCount =
    primStr (caPre <> content <> caPost) (Just (length content))
  where
    caPre, caPost :: String
    caPre = "^ca(1, xdg-open https://mail.google.com/)"
    caPost = "^ca()"
    content :: String
    content =
      case mCount of
        Nothing -> "----"
        Just count ->
          if count > 9999
            then ">=1k"
            else fromString (printf "%4d" count)

renderMpd :: Maybe Mpd.State -> DString
renderMpd mpdSt = "[" <> st <> "]"
  where
    st =
      case mpdSt of
        Nothing -> "?"
        Just Mpd.Playing -> ">"
        Just Mpd.Stopped -> "|"
        Just Mpd.Paused -> "|"

renderBattery :: (Int, Bool) -> DString
renderBattery (capa, charge) = chgRdr <> capRdr
  where
    chgRdr = if charge then "+" else "="
    capRdr =
      if capa == 100
        then "Ful"
        else fromString (printf "%2d%%" capa)

renderDateTime :: (String, String) -> DString
renderDateTime (dateStr, timeStr) = dStr <> " " <> tStr
  where
    dStr = fg (sRGB24read "#80FFFF") (fromString dateStr)
    tStr = fg (sRGB24read "#FFFF80") (fromString timeStr)

render :: forall w. Worker w => Proxy w -> WStateRep w -> DString
render p st =
    -- try typecasting and pick first one that has succeeded
    fromMaybe fallback (getAlt (foldMap Alt handlers))
  where
    handlers :: [Maybe DString]
    handlers =
        [ eqT @w @CpuUsage <&>
            \Refl -> renderCpuUsage st
        , eqT @w @CpuMaxFreq <&>
            \Refl -> renderCpuMaxFreq st
        , eqT @w @MemUsage <&>
            \Refl -> renderMemUsage st
        , eqT @w @TopProc <&>
            \Refl -> renderTopProc st
        , eqT @w @NetStat <&>
            \Refl -> renderNetStat st
        , eqT @w @Mail <&>
            \Refl -> renderMail st
        , eqT @w @Mpd <&>
            \Refl -> renderMpd st
        , eqT @w @Battery <&>
            \Refl -> renderBattery st
        , eqT @w @DateTime <&>
            \Refl -> renderDateTime st
        ]

    fallback :: DString
    fallback = fromString (show (typeRep p))
