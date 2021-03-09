# biparsing

Biparsing is a bidirectional programming technique that specializes in constructing parsing and serialization programs simultaneously.

## Objectives

Find a suitable methodology to allow biparsing to be implemented in common programming languages.

In the meantime, document and critique bidirectional programming ideas, implementations, and papers with a focus on parsing streams of objects and reversing the production back into the stream..

## Current Implementations and their Problems

### Applicative Functor Composition

Currently, a viable solution for constructing biparsers. The drawback is that it is hard to modify since it requires that the parser constructor function must be defined separately from the parsing logic. Also, for serialization an extractor function is required to remove what is to be serialized from the parse type.

```haskell
data Person = Person String String

-- constructor function
person :: String -> String -> Person
person f l = Person f l

-- extractor functions
firstName :: Person -> String
firstName (Person x _) = x
lastName :: Person -> String
lastName (Person _ x) = x

personBiparser :: Biparser String Person
personBiparser = 
  person <$> -- sets constructor function
  many firstName alpha <*> -- Parsing: `many` passes the consumed alpha characters as the first argument of the `person` constructor function. Serializing: `many` uses the extractor function `firstName` to get the first name charcaters from a `Person` instance and passes each single caracter to the alpha serializer
  (spaces *> many lastName alpha) -- Parsing: `spaces` skips the spaces, `many` passes the consumed alpha characters as the second argument of the `person` constructor function. Serializing: `spaces` writes a " " space. `many lastName alpha` does what the above line does.

spaces :: Biparser a String
spaces = many (const [' ']) space

many :: (a -> [b]) -> Biparser b c -> Biparser a [c]
many extractor biparser = ...
```

Another, example from the linked **codec** package.
```haskell
data RecordB = RecordB
  { recordBString :: String
  , recordBDouble :: Double
  } deriving (Eq, Ord, Show)

recordBObjCodec :: JSONCodec RecordB
recordBObjCodec = asObject "RecordB" $
  RecordB
    <$> recordBString =. field "string"
    <*> recordBDouble =. field "double"
```

### Monadic Composition

Monadic composition does not have a valid implementation yet but does have an example implementation **unparse-attoparsec**, details below, that is outlined in [Composing bidirectional programs monadically](./papers/1902.06959.pdf).f

```haskell
int :: Biparser Int Int
int = do
  let printedInt n = show n ++ " "
  ds <- digits 'upon' printedInt
  return (read ds)

digits :: Biparser String String
digits = do
  d <- char 'upon' head
  if isDigit d then do
    igits <- digits 'upon' tail
    return (d : igits)
  else if d == ' ' then return " "
  else error "Expected digit or space"
```

### Optics Compositon

Optics compostion can be used to create bidirectional programs by utilizing Iso (isomorphisms) and Prisms. These are ready to go and can be used to create composable bidirectional programms. Unfortuantely at this point, optics do not support state very well so it's difficult to report errors with locations.

## Required Features

- Disjoint ordering of the data Constructors and the parsing order. Monadic parsers assign parsed values to variables that can then be used in any order to construct the final data instance. Many biparsers us Applicative Functors which require the parse order to match the data constructor arguments.
- Polymorphic Streams and Token types. This should not just be for parsing Text, String, or ByteString streams but any kind of tokenized streams.
- Parser composition. If the parser consumes stream 'a' and produces stream 'b' it should be composable with another parser that consumes stream 'b'.
- Ability to have guaranteed serialization. Parsing needs to be able to fail.

## Chat
Active conversations on https://funprog.zulipchat.com/#narrow/stream/225296-Biparsing

## Reference Links
- [List Relevant References](https://github.com/BebeSparkelSparkel/biparsing/issues/1)
- [Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing](./papers/rendel10invertible.pdf)
- [Composing bidirectional programs monadically](./papers/1902.06950.pdf)
- [Generalized Convolution and Efficient Language Recognition](http://conal.net/papers/convolution/) by Conal Elliott

# Notable Quotes

"IMO it's a mistake (although not an infrequent one) to try and use an optic as a parser or a printer or whatever. It's like trying to light a fire by striking your lighter against a rock. The whole point of polymorphic profunctor optics is that they can adjust arbitrary profunctors, including natural representations of parsers and printers"
**masaeedu**

"I suspect a "clean" way to do this sort of thing will eventually reveal within a profunctor encoding, but I haven't put much time into it yet :)" **Chris Penner**

"Are there any bidirectional parser libraries ready to use?" **TheMatten**

"None that are good.
Well "good" is not the right word here
Basically the best current approach to bidirectional parsing in Haskell does not have the ideal interface"
**chessai**

# Existing Implementations

## [codec](https://hackage.haskell.org/package/codec) Simple bidirectional serialization
Codec makes it simple to write composable bidirectional serializers with a consistent interface.

Pros
- JSON and binary supported natively
- Allows any kind of token not just Char and Word8

Cons
- Does not give any helper functions for common parsing needs like parsec-combinators
- More could be handled in the type level
- Dependant on the constructor argument order. The constructor arguments need to be reordered if the parser changes. Type errors are not clear as to which field there is a problem parsing for.
- Does not easily support ADTs

Features we would like to also have
- ...

## [lens](https://hackage.haskell.org/package/lens) Lenses, Folds and Traversals

Pros
- ...

Cons
- ...

Features we would like to also have
- ...

## [boomerang](https://hackage.haskell.org/package/boomerang) Library for invertible parsing and printing
Specify a single unified grammar which can be used for parsing and pretty-printing

Pros
- Supports ADTs
- Allows any token type
- Natively supports String and Text

Cons
- Uses lots of Template Haskell
- Mostly for use with Text

Features we would like to also have
- ...

Based On
- Zwaluw by Sjoerd Visscher and Martijn van Steenbergen
- invertible-syntax

## [Zwaluw](http://hackage.haskell.org/package/Zwaluw)

Pros
- ...

Cons
- ...

Features we would like to also have
- ...

## [sexp-grammar](http://hackage.haskell.org/package/sexp-grammar)
Grammar combinator framework to build invertible parsing libraries for concrete syntax.

Contains [invertible-syntax](http://hackage.haskell.org/package/invertible-syntax)

Pros
- ...

Cons
- ...

Features we would like to also have
- ...

Based On
- Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing by Tillmann Rendel and Klaus Ostermann

## [roundtrip](https://hackage.haskell.org/package/roundtrip) Bidirectional (de-)serialization
Roundtrip allows the definition of bidirectional (de-)serialization specifications. The specification language is based on the ideas described in the paper Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing by Tillmann Rendel and Klaus Ostermann, Haskell Symposium 2010.
This package does not provide concrete instances of the specification classes, see the packages roundtrip-string and roundtrip-xml instead.
The package contains slightly modified code from Tillmann Rendel's partial-isomorphisms and invertible-syntax packages (Copyright (c) 2010-11 University of Marburg).

Pros
- ...

Cons
- ...

Features we would like to also have
- ...

Based On
- Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing by Tillmann Rendel and Klaus Ostermann
- partial-isomorphisms by Tillmann Rendel
- invertible-syntax

## [unparse-attoparsec](https://github.com/Lysxia/unparse-attoparsec)
This library provides a wrapper [attoparsec](https://hackage.haskell.org/package/attoparsec) to build programs that can be interpreted both as parsers and as printers.

Pros
- ...

Cons
- Only supports ByteString

Features we would like to also have
- ...

# Implementation Critiques

## [Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing](./papers/rendel10invertible.pdf)

Partial isomorphisms is the basis of this implementation `data Iso a b = Iso (a -> Maybe b) (b -> Maybe a)` and redefining `<$>`, `<*>`, `<|>` to suite partial `Iso`. The reader may be wondering why it is partial in both directions since it is strange that serialization could fail. This is due to the fact that `<|>` Alternative does not force the serialization of all possibilities at the type level so failure must be passed into runtime.

```haskell
data Iso a b = Iso (a -> Maybe b) (b -> Maybe a)

class IsoFunctor f where
  (<$>) :: Iso a b -> f a -> f b

class ProductFunctor f where
  (<*>) :: f a -> f b -> f (a,b)

class Alternative f where -- lacks Applicative superclass
  (<|>) :: f a -> f a -> f a
  empty :: f a -- fails parsing and printing
```

I would have preferred that the definition of `data Iso a b = Iso (a -> Maybe b) (b -> a)` have been used instead to force successful serialization. This would however put more complexity in the types to enforce this.
Also, due to using applicative the order of the parsing must coincide with the constructor arguments. With monadic parsers the parsed value can be assigned to a variable which disconnects the constructor argument order and the parser order.
Type lists may be able to enforce: guaranteed serialization with Alternative, and unordered parsing.

## [Composing bidirectional programs monadically](./papers/1902.06959.pdf)

