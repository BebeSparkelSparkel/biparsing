{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
import Biparse.BiparserSpec qualified


main :: IO ()
main = hspec do
  describe "forward" $
    applyType @Combinations Biparse.BiparserSpec.specForward (*>) $ pure ()

  --  Biparse.BiparserSpec.specForward (Proxy @(Fwd (StateErrorT (Position UnixLC (), String) (Either ((Position UnixLC (), String), String)))))
  --  Biparse.BiparserSpec.specForward @(Fwd (StateT (Position UnixLC (), String) IO))
  --  Biparse.BiparserSpec.specForward @(Fwd (StateT (Position UnixLC (), String) IO))
  --  Biparse.BiparserSpec.specForward @(Fwd (FileT String IO))
  --  Biparse.BiparserSpec.specForward @(Fwd (FileT String (UpdateState (StateT (Position UnixLC FilePath) IO))))

  --  Biparse.BiparserSpec.specForward @(Fwd (StateErrorT (Position UnixLC (), Text) (Either ((Position UnixLC (), Text), String))))
  --  Biparse.BiparserSpec.specForward @(Fwd (StateT (Position UnixLC (), Text) IO))
  --  Biparse.BiparserSpec.specForward @(Fwd (StateT (Position UnixLC (), Text) IO))
  --  Biparse.BiparserSpec.specForward @(Fwd (FileT Text IO))
  --  Biparse.BiparserSpec.specForward @(Fwd (FileT Text (UpdateState (StateT (Position UnixLC FilePath) IO))))

  --describe "backward" do
  --  Biparse.BiparserSpec.specBackward @(Bwd (WriterT String (Either String)))
  --  Biparse.BiparserSpec.specBackward @(Bwd (WriterT String IO))
  --  Biparse.BiparserSpec.specBackward @(Bwd (FileT String IO))
  --  Biparse.BiparserSpec.specBackward @(Bwd (FileT String (StateT () IO)))

  --  Biparse.BiparserSpec.specBackward @(Bwd (WriterT Text (Either String)))
  --  Biparse.BiparserSpec.specBackward @(Bwd (WriterT Text IO))
  --  Biparse.BiparserSpec.specBackward @(Bwd (FileT Text IO))
  --  Biparse.BiparserSpec.specBackward @(Bwd (FileT Text (StateT () IO)))

type ForwardProfunctor :: transformer -> Type {- read -} -> Type {- write -} -> state -> monad -> Type {- text -} -> Type -> Type -> Type
type ForwardProfunctor transformer read write state monad text =
 Fwd
 ( MakeTransMonad
   transformer
   read
   write
   (MakeState transformer state text)
   ( MakeMonad
     transformer
     read
     write
     (MakeState transformer state text)
     (MakeSubMonad transformer read write (MakeState transformer state text) monad)
   )
   text
 )

type MakeTransMonad :: transformer -> Type -> Type -> Type -> (Type -> Type) -> Type -> (Type -> Type)
type family MakeTransMonad transformer read write state monad text where
  MakeTransMonad FileT  _ _ _ m text  = FileT text m
  MakeTransMonad rwsT   r w s m _     = rwsT r w s m
  MakeTransMonad stateT _ _ s m _     = stateT s m

type MakeState :: transformer -> state -> Type {- text -} -> Type
type family MakeState transformer state text where
  MakeState FileT s _ = s
  MakeState _ s t = (s, t)

type MakeMonad :: transformer -> Type {- read -} -> Type {- write -} -> Type {- state -} -> monad -> (Type -> Type)
type family MakeMonad transformer read write state monad where
  MakeMonad FileT   _ _ _ IO      = IO
  MakeMonad FileT   r w s rwsT    = UpdateState (rwsT r w s IO)
  MakeMonad FileT   _ _ s stateT  = UpdateState (stateT s IO)
  MakeMonad _       _ _ _ m       = m
  --MakeMonad rwsT   r w s _ m      = rwsT r w s m
  --MakeMonad stateT _ _ s _ m      = stateT s m

type MakeSubMonad :: transformer -> Type {- read -} -> Type {- write -} -> Type {- state -} -> monad -> (Type -> Type)
type family MakeSubMonad transformer monad read write state where
  MakeSubMonad FileT  _ _ _ IO      = IO
  MakeSubMonad FileT  r w s m       = MakeTransMonad m r w s IO ()
  MakeSubMonad _      _ _ s Either  = Either s
  MakeSubMonad _      _ _ _ IO      = IO

type Transformers :: [Type]
type Transformers =
  --'[Poly RWST
  '[ Poly StateT
  ]

type Reads :: [Type]
type Reads =
  '[ ()
  ]

type Writes :: [Type]
type Writes =
  '[ ()
  ]

type States :: [Type]
type States =
  '[ Position UnixLC ()
  --, IndexPosition ()
  ]

type Monads :: [Type]
type Monads =
  '[Poly IO
  --, Poly Either
  ]

type Texts :: [Type]
type Texts =
  '[String
  --, Text
  ]

type Poly :: k -> Type
data Poly k

type Pairs :: [k] -> [l] -> [(k,l)] -> [(k,l)]
type family Pairs xs ys zs where
  Pairs (x ': xs) ys  zs = Associate x ys (Pairs xs ys zs)
  Pairs '[]       _   zs = zs
type Associate :: k -> [l] -> [(k,l)] -> [(k,l)]
type family Associate x ys zs where
  Associate x (y ': ys) zs = '(x, y) ': Associate x ys zs
  Associate _ '[]       zs = zs

type Combinations = Pairs Transformers (Pairs Reads (Pairs Writes (Pairs States (Pairs Monads Texts '[]) '[]) '[]) '[]) '[]

type ApplyType :: [(Type, (Type, (Type, (Type, (Type, Type)))))] -> Constraint
class ApplyType ts where
  applyType
    :: ( forall (p :: Type -> Type -> Type) m c.
      ( forall a. ShouldFail (m a)
      , forall u. Applicative (p u)
      , forall u. Peek (p u)
      , forall u. Try (p u)
      , forall u. Alt (p u)
      , forall u. MonadFail (p u)
      , forall v. Eq v => Eq (c v)
      , forall v. Show v => Show (c v)
      , forall v. MakeForwardResult (Position () FilePath -> IndexPosition FilePath -> String -> v -> c v)
      , Profunctor p
      , Typeable p
      , One Char p
      , RunForward p FilePath String m c
      , ShouldReturn m
      ) => Proxy p -> b)
    -> (b -> b -> b)
    -> b
    -> b
--instance ApplyType (t ': ts) => ApplyType ( '(Poly transformer, '(read, '(write, '(state, '(monad, text))))) ': t ': ts ) where
--  applyType f g x = f (Proxy :: Proxy (ForwardProfunctor transformer read write state monad text)) `g` applyType @(t ': ts) f g x
instance
  ( RunForward p FilePath String m c
  , p ~ ForwardProfunctor transformer read write state monad text
  , tm ~ MakeTransMonad transformer read write s sm text
  , sm ~ MakeSubMonad transformer read write s monad
  , s ~ MakeState transformer state text
  , forall a. ShouldFail (m a)
  , MonadFail tm
  , Peek tm
  , Try tm
  , Alt tm
  , Typeable tm
  , OneFwd Char tm
  , forall v. MakeForwardResult (Position () FilePath -> IndexPosition FilePath -> String -> v -> c v)
  , forall v. Eq v => Eq (c v)
  , forall v. Show v => Show (c v)
  , ShouldReturn m
  ) => ApplyType '[ '(Poly transformer, '(read, '(write, '(state, '(Poly monad, text))))) ] where
  applyType f g x = f (Proxy :: Proxy p) `g` x

