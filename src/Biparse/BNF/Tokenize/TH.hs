{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}

module Biparse.BNF.Tokenize.TH
  ( take'
  , count'
  , constructorList
  ) where

import Biparse.AlternativeAttributes (addAtt)
import Biparse.BNF.Tokenize.Type (Token')
import Biparse.General (takeDi, count)
import Data.Char (Char)
import Data.Data (showConstr, toConstr)
import Data.Foldable (foldr)
import Data.Function (($), (.), id)
import Data.Function (flip)
import Data.Kind qualified as K
import Data.Monoid (mempty)
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (D1, C1, S1, U1, Meta(MetaCons), (:+:), Rep, Rec0, (:*:))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Language.Haskell.TH (ExpQ, TypeQ, mkName, conE, litE, charL, promotedT, conT, infixT, promotedNilT, Name, appT, conP, varP, varT)
import Unsafe.Coerce (unsafeCoerce)

-- | Resolves to @:: AlternativeAttributes '[t] isoMT@
take' :: forall text count. Char -> Token' text count -> ExpQ
take'
  (litE . charL -> c)
  t
  = [e| addAtt @($(tokenKind t)) (takeDi @($cT) @($textT) $c $(token t)) |]
  where
  cT = varT $ mkName "c" :: TypeQ
  textT = varT $ mkName "text" :: TypeQ

count' :: forall text count. Char -> Token' text count -> ExpQ
count'
  (litE . charL -> c)
  t
  = [e| addAtt @($cnT K.Type) ($cnE <$> count $c `uponM` \case $pat -> pure x; _ -> empty) |]
  where
  cnT = promotedT cn
  cnE = conE cn
  cn = conName t :: Name
  pat = conP cn [varP $ mkName "x"]

--addAtt :: ExpQ
--addAtt = varE $ mkName $ "addAtt"

tokenKind :: Token' text count -> TypeQ
tokenKind = promotedT . conName

token :: Token' text count -> ExpQ
token = conE . conName

conName :: Token' text count -> Name
conName t = mkName $ showConstr $ toConstr $ (unsafeCoerce t :: Token' () ())

-- | Returns a promoted list
constructorList :: forall a. CL (Rep a) => TypeQ
constructorList = foldr (\x y -> x `infixT` colon $ y) promotedNilT $ cl @(Rep a) mempty
class CL (a :: K.Type -> K.Type) where cl :: [TypeQ] -> [TypeQ]
instance CL cs => CL (D1 a cs) where cl = cl @cs
instance (CL a, CL b) => CL (a :+: b) where cl = cl @a . cl @b
instance (AddArgs args, KnownSymbol conName) => CL (C1 ('MetaCons conName a b) args) where
  cl = (:) $ addArgs @args $ promotedT $ symbol @conName
class AddArgs (a :: K.Type -> K.Type) where addArgs :: TypeQ -> TypeQ
instance AddArgs (S1 a (Rec0 K.Type)) where
  addArgs = flip appT $ conT ''K.Type
--instance {-# OVERLAPPABLE #-} Typeable t => AddArgs (S1 a (Rec0 t)) where
--  addArgs = flip appT $ conT $ mkName $ tyConName $ typeRepTyCon $ typeRep @t
instance (AddArgs a, AddArgs b) => AddArgs (a :*: b) where
  addArgs = addArgs @b . addArgs @a
instance AddArgs U1 where addArgs = id

--constructorList :: forall a. ConstructorList (Rep a) => TypeQ
--constructorList = foldr (\x y -> mkType x `infixT` colon $ y) promotedNilT $ constructorList' @(Rep a) mempty
--  where
--  mkType (conName, 0) = conT conName
--  mkType (conName, x) = mkType (conName, x - 1) `appT` type'
--
--class ConstructorList (a :: K.Type -> K.Type) where constructorList' :: [(Name,Int)] -> [(Name,Int)]
--instance ConstructorList cs => ConstructorList (D1 a cs) where constructorList' = constructorList' @cs
--instance (ConstructorList a, ConstructorList b) => ConstructorList (a :+: b) where
--  constructorList' = constructorList' @a . constructorList' @b
--instance (CountArgs args, KnownSymbol conName) => ConstructorList (C1 ('MetaCons conName a b) args) where
--  constructorList' = (:) (symbol @conName, countArgs @args)
--class CountArgs (a :: K.Type -> K.Type) where countArgs :: Int
--instance CountArgs U1 where countArgs = 0
--instance CountArgs (S1 a b) where countArgs = 1

colon :: Name
colon = mkName ":"

symbol :: forall (a :: Symbol). KnownSymbol a => Name
symbol = mkName $ symbolVal $ Proxy @a

