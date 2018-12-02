module XMonad.Javran.SysInfoBar.TH where

import Language.Haskell.TH
import Data.Maybe

genWorkers :: Q Exp
genWorkers = do
    (Just subcmdTypeName) <- lookupTypeName "Worker"
    ClassI _ ins <- reify subcmdTypeName
    (Just proxyTypeName) <- lookupTypeName "Proxy"
    (Just proxyValName) <- lookupValueName "Proxy"
    (Just esubValName) <- lookupValueName "EWorker"
    let typNames = mapMaybe getTypes ins
        gen :: Name -> Exp
        gen n = AppE (ConE esubValName) inner
          where
            innerV = ConE proxyValName
            innerT = AppT (ConT proxyTypeName) (ConT n)
            inner = innerV `SigE` innerT
    pure (ListE (gen <$> typNames))
  where
    getTypes :: InstanceDec -> Maybe Name
    getTypes (InstanceD _ _ (AppT _ (ConT tyN)) _) = Just tyN
    getTypes _ = Nothing
