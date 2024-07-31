module Biparse.Core.Aliases (
Biparser,
Iso,
Unit,
unit,
Const,
ConstU,
) where

-- | Alias just to indicate that the profunctor is a biparser
type Biparser :: (Type -> Type -> Type) -> Type -> Type -> Type
type Biparser p = p

-- | Iso when @u ~ v@
type Iso :: (Type -> Type -> Type) -> Type -> Type
type Iso p v = Biparser p v v

-- | Unit when @u@ and @v@ are @()@
type Unit p = Biparser p () ()

-- | Throws away @u@ and @v@
unit :: forall p u. Profunctor p => Unit p -> Const p u
unit = lmap $ const ()

-- | Discards @u@ and returns ()
type Const p u = Biparser p u ()

-- | Discards @u@
type ConstU p u v = Biparser p u v

