{-# LANGUAGE
    ExplicitForAll
  , LambdaCase
  , ScopedTypeVariables
  , OverloadedStrings
  , TypeOperators
  , TypeFamilies
  , TypeApplications
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

import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.SysInfoBar.CpuUsage (CpuUsage)
import XMonad.Javran.SysInfoBar.CpuMaxFreq (CpuMaxFreq)

{- TODO
import XMonad.Javran.SysInfoBar.MemUsage (MemUsage)
import XMonad.Javran.SysInfoBar.TopProc (TopProc)
import XMonad.Javran.SysInfoBar.NetStat (NetStat)
import XMonad.Javran.SysInfoBar.Mail (Mail)
import XMonad.Javran.SysInfoBar.Mpd (Mpd)
import XMonad.Javran.SysInfoBar.Battery (Battery)
import XMonad.Javran.SysInfoBar.DateTime (DateTime)
-}

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
        ]
        
    fallback :: DString
    fallback = fromString (show (typeRep p))
