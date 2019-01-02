{-# LANGUAGE
    ExplicitForAll
  , LambdaCase
  , ScopedTypeVariables
  , OverloadedStrings
  , TypeOperators
  , TypeFamilies
  #-}
module XMonad.Javran.SysInfoBar.DzenRender where

import System.Dzen
import Data.Proxy
import Data.String
import Data.Typeable
import Data.Monoid
import Data.Colour.Names
import Data.Maybe
import Control.Monad

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

render :: forall w. Worker w => Proxy w -> WStateRep w -> DString
render p st = fromMaybe fallback (getAlt (Alt result))
  where
    fallback :: DString
    fallback = fromString (show (typeRep p))
    result :: Maybe DString
    result = (eqT :: Maybe (w :~: CpuUsage)) >>= \case
      Refl -> Just (renderCpuUsage st)

