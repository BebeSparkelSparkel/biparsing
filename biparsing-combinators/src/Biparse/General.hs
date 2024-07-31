module Biparse.General (
take,
takeUnit,
takeUni,
takeDi,
takeTri,
takeNot,
--takeWhile,
takeDi',
takeTri',
--drop,
--dropWhile,
--dropUntil,
--skipUntil,
--untilJust,
--takeN,
--pad,
--padSet,
--padCount,
--breakWhen,
--breakWhen',
--breakAt,
--optionMaybe,
optional,
EqualityWrapper,
StripPrefixEqualityCheck,
Length(length),
stripPrefix,
--countElement,
--countElementSome,
--not,
failBool,
--memptyWrite,
--rest,
--shouldFail,
) where

-- * take for single elements

-- | Assumes but disregards the writer context
take :: forall p u item.
  ( Profunctor p
  , Try (p item)
  , One item p
  , forall u'. MonadFail (p u')
  , Eq item
  , Show item
  )
  => item
  -> Const p u
take takeWrite = unit $ takeUnit takeWrite

-- | Discards the match
takeUnit :: forall p item.
  ( Profunctor p
  , Try (p item)
  , One item p
  , forall u. MonadFail (p u)
  , Eq item
  , Show item
  )
  => item
  -> Unit p
takeUnit takeWrite = void $ takeUni takeWrite `upon` const takeWrite

-- | Returns the match
takeUni :: forall p item.
  ( Try (p item)
  , One item p
  , MonadFail (p item)
  , Eq item
  , Show item
  )
  => item
  -> Iso p item
takeUni takeWriteMatchReturn = try do
  y <- one
  if takeWriteMatchReturn == y
  then return y
  else expectedFail takeWriteMatchReturn y

takeDi :: forall p m u item.
  ( Colift p m
  , MonadFail m
  , MonadFail (p u)
  , Try (p u)
  , One item p
  , Eq item
  , Show item
  , Eq u
  , Show u
  )
  => item
  -> u
  -> Iso p u
takeDi takeWrite matchReturn = takeTri takeWrite matchReturn matchReturn

-- | Allows 'SubElement s'`, 'u', and 'v' to be fixed which works well with Alternative.
takeTri :: forall p m u item v.
  ( Colift p m
  , MonadFail m
  , MonadFail (p u)
  , Try (p u)
  , One item p
  , Eq item
  , Show item
  , Eq u
  , Show u
  )
  => item
  -> u
  -> v
  -> Biparser p u v
takeTri takeWrite toMatch toReturn = try do
  x <- one `uponM` ($> takeWrite) . \x -> bool (expectedFail toMatch x) (pure ()) $ x == toMatch
  if takeWrite == x
  then return toReturn
  else expectedFail takeWrite x

expectedFail :: (MonadFail m, Show a, Show b) => a -> b -> m c
expectedFail x y = fail $ "Expected a " <> show x <> " but received a " <> show y

takeNot :: forall p item.
  ( One item p
  , MonadFail (p item)
  , Try (p item)
  , Show item
  , Eq item
  )
  => item
  -> Iso p item
takeNot x = try do
  y <- one
  if x == y
  then pure y
  else fail $ "Should not have found an " <> show y

-- * Take for prefixes

takeDi' :: forall p m u seq.
  ( Colift p m
  , MonadFail m
  , Profunctor p
  , BiN p
  , Try (p u)
  , Monad (p u)
  , Try (p ())
  , MonadFail (p ())
  , Eq u
  , Show u
  , Length seq
  , Show seq
  , Applicative (EqualityWrapper (StripPrefixEqualityCheck p))
  , Eq (EqualityWrapper (StripPrefixEqualityCheck p) seq)
  )
  => seq
  -> u
  -> Iso p u
takeDi' takeWrite matchReturn = takeTri' takeWrite matchReturn matchReturn

-- | Allows 'SubState c s'`, 'u', and 'v' to be fixed which works well with Alternative.
takeTri' :: forall p m u v seq.
  ( Colift p m
  , MonadFail m
  , Profunctor p
  , BiN p
  , Try (p u)
  , Monad (p u)
  , Try (p ())
  , MonadFail (p ())
  , Eq u
  , Show u
  , Length seq
  , Show seq
  , Applicative (EqualityWrapper (StripPrefixEqualityCheck p))
  , Eq (EqualityWrapper (StripPrefixEqualityCheck p) seq)
  )
  => seq
  -> u
  -> v
  -> Biparser p u v
takeTri' takeWrite toMatch toReturn = try do
  stripPrefix takeWrite `uponM` \x -> bool (expectedFail toMatch x) (pure ()) $ x == toMatch
  return toReturn

---- * Take while predicate
--
--takeWhile :: forall c s p n seq item w.
--  ()
--  => (item -> Bool)
--  -> Iso p seq
--takeWhile = undefined
----takeWhile = split . state @c . span
--
--drop :: forall c s p n u v e.
--  ()
--  => Biparser p u v
--  -> Biparser p u ()
--drop = undefined
----drop bp = ignoreBackward () $ try bp *> drop bp <!> pure ()
--
---- | Drop forward elements while predicate is true.
--dropWhile :: forall c s p n u seq item w.
--  ()
--  => (item -> Bool)
--  -> Const p u
--dropWhile = undefined
----dropWhile f = splitFw $ stateT @c $ return . MT.span f
--
---- | Drop forward elements until @item@ is found. Fails if @item@ is not found.
--dropUntil :: forall c s p n u seq item w e.
--  ()
--  => item
--  -> Const p u
--dropUntil = undefined
----dropUntil x = dropWhile (/= x) <* take x
--
---- | Run until returns True
--skipUntil :: forall c s p n u.
--  ()
--  => Biparser p u Bool
--  -> Const p u
--skipUntil = undefined
----skipUntil x = bool (void $ skipUntil x) (pure ()) =<< resetState id x
--
---- | Run until Just
--untilJust :: forall c s p n u a seq.
--  ()
--  => Biparser p u (Maybe a)
--  -> Biparser p u a
--untilJust = undefined
----untilJust x = maybe (untilJust x) pure =<< x
--
---- * N elements
--
---- | Take n elements. Does not limit what is written backwards.
--takeN :: forall c s p n seq w.
--  ()
--  => Int
--  -> Iso p seq
--takeN = undefined
----takeN = split . stateT @c . (return .) . MT.splitAt
--
---- * Pad
--
---- | Consumes the pad 'c' charcaters forward. Prepends the pad 'c' caracters backwards to ensure there are 'n' charcaters written.
---- Probably best to roll your own if using a writer type like 'String' that has as slow length function.
--pad :: forall c s p n u v seq i item w j.
--  ()
--  => Int
--  -> item
--  -> Biparser p u v
--  -> Biparser p u v
--pad = undefined
----pad n c = padTemplate (== c) n c
--
--padSet :: forall c s p n u v seq i item w j.
--  ()
--  => Int
--  -> item
--  -> [item]
--  -> Biparser p u v
--  -> Biparser p u v
--padSet = undefined
----padSet n c cs = padTemplate (`member` cs) n c
--
--padTemplate :: forall c s p n u v seq i item w j.
--  ()
--  => (item -> Bool)
--  -> Int
--  -> item
--  -> Biparser p u v
--  -> Biparser p u v
--padTemplate = undefined
----padTemplate dropPred n c x = do
----  dropWhile dropPred
----  mapWrite x \y ->
----    let l = lengthIndex y
----    in if l >= n
----      then y
----      else replicate (n - l) c <> y
--
---- | Gives the pad count found for forward (number of c + number consumed by x). Just returns n backwards.
--padCount :: forall c s p n u v seq i item w j.
--  ()
--  => Int
--  -> item
--  -> Biparser p u v
--  -> Biparser p u (Int, v)
--padCount = undefined
----padCount n c x = endoSecond (first $ const $ fromIntegral n) $ count $ pad n c x
--
---- * Break
--
---- | Breaks off the substate head when 'x' succeeds. Writes x after given 'ss'.
---- DEV NOTE: Seems like there could be a more simple solution.
--breakWhen :: forall c s p n seq item w e.
--  ()
--  => Unit n
--  -> Iso p seq
--breakWhen = undefined
----breakWhen (Biparser fw bw) = Biparser
----  fw'
----  \u -> do
----    tell =<< convertSequence @c u
----    bw ()
----    return u
----  where
----  fw' = mempty <$ fw <!> cons <$> oneFw @c <*> fw'
--
---- | 'x' does not succeed
--breakWhen' :: forall c s t p seq w e.
--  ()
--  => Unit p
--  -> Iso p seq
--breakWhen' = undefined
----breakWhen' (Biparser fw bw) = Biparser fw' bw'
----  where
----  fw' = do
----    startState <- get
----    let its = initTails $ getSubState @s startState
----    tryState $ maybe (fail "Could not find break.") (pure . fst) =<< flip findM its \(h,t) -> do
----      put $ updateSubStateContext @c startState h t
----      fw $> True <!> pure False
----  bw' x = do
----    tell =<< convertSequence @c x
----    bw ()
----    return x
------ | Like 'breakWhen' but fails if 'x' does not succeed
----breakWhen' :: forall c s m n seq.
----  BreakWhen c s m n seq
----  => Unit n
----  -> Iso m seq
----breakWhen' x
----  = bw
----  <!> ignoreForward  (write *> unit x)
----  where
----  bw
----    =   mempty <$ (failBackward $ try $ unit x)
----    <!> do
----          y <- one `uponM` headAlt
----          cons y <$> breakWhen' x `uponM` tailAlt
--
--breakAt :: forall c s p n seq item w e.
--  ()
--  => item
--  -> Iso p seq
--breakAt = undefined
----breakAt = breakWhen . take
--
---- | Consumes rest/all of substate and writes given
--rest :: forall c s p n seq w.
--  ()
--  => Iso p seq
--rest = undefined
----rest = split $ get <* put mempty
--
---- | The forward of the given biparser should fail. If it does not fail, fail with the given string.
---- Backwards is ignored.
--shouldFail :: forall c s p n u u' v.
--  ()
--  => Biparser p u v
--  -> String
--  -> Biparser p u' ()
--shouldFail = undefined
----shouldFail bp msg = ignoreBackward () $ peek $ bool (pure ()) (fail msg) =<< failBool bp
--
---- * Optional parsing
--
--optionMaybe :: forall c s p n u v e.
--  ()
--  => Biparser p u v
--  -> Biparser p u (Maybe v)
--optionMaybe = undefined
----optionMaybe x = Just <$> try x <!> pure Nothing

-- | Allows a parser to fail and return Maybe instead. Allows writer to optionally run or not.
optional :: forall p m u v.
  ( forall u'. Functor (p u')
  , forall u'. Alt (p u')
  , forall u'. Applicative (p u')
  , Try (p u)
  , Colift p m
  , MonadFail m
  ) 
  => Biparser p u v
  -> Biparser p (Maybe u) (Maybe v)
optional x = Just <$> try x `uponM` maybe (fail "") pure <!> pure Nothing

-- * Stripping

data EqualityCheck = CheckEquality | NoEqualityCheck

type EqualityWrapper :: EqualityCheck -> Type -> Type
type family EqualityWrapper a where
  EqualityWrapper 'CheckEquality = Identity
  EqualityWrapper 'NoEqualityCheck = Proxy

type StripPrefixEqualityCheck :: (Type -> Type -> Type) -> EqualityCheck
type family StripPrefixEqualityCheck p
type instance StripPrefixEqualityCheck (Fwd _) = 'CheckEquality
type instance StripPrefixEqualityCheck (Bwd _) = 'NoEqualityCheck

class Length seq where length :: seq -> Int

stripPrefix :: forall p seq u eq.
  ( Profunctor p
  , BiN p
  , Try (p u)
  , MonadFail (p u)
  , Length seq
  , Show seq
  , Applicative (EqualityWrapper (StripPrefixEqualityCheck p))
  , eq ~ EqualityWrapper (StripPrefixEqualityCheck p) seq
  , Eq eq
  )
  => seq
  -> Const p u
stripPrefix prefix = try do
  xs <- biN (length prefix) `uponConst` prefix
  unless ((pure prefix :: eq) == pure xs) $ fail $ "Could not match prefix: " <> show prefix

---- * Counting
--
---- | Counts 0 or more elements
--countElement :: forall c s p n seq item w.
--  ()
--  => item
--  -> Iso p Int
--countElement = undefined
----countElement x = toEnum . length <$> takeWhile (== x) `upon` flip replicate x . fromNatural
--
---- | Counts 1 or more elements
--countElementSome :: forall c s p n seq item w.
--  ()
--  => item
--  -> Biparser p Int Int
--countElementSome = undefined
----countElementSome x = do
----  c <- countElement x
----  unless (c > 0) $ fail "countElementSome expected at least one element match but found none"
----  return c
--
--not :: forall c s p n u.
--  ()
--  => Biparser p u Bool
--  -> Biparser p u Bool
--not = undefined
----not = fmap Data.Bool.not

-- | If success, returns True. If fails, returns False
failBool :: (Applicative m, Alt m) => m a -> m Bool
failBool x = x $> True <!> pure False

---- | Causes backward to write nothing.
--memptyWrite :: forall c s p n u v w.
--  ()
--  => Biparser p u v
--  -> Biparser p u v
--memptyWrite = undefined
----memptyWrite = flip mapWrite (const mempty)
