# biparsing

## Reference Links
See issue https://github.com/BebeSparkelSparkel/biparsing/issues/1

## Notable Quotes

"IMO it's a mistake (although not an infrequent one) to try and use an optic as a parser or a printer or whatever. It's like trying to light a fire by striking your lighter against a rock. The whole point of polymorphic profunctor optics is that they can adjust arbitrary profunctors, including natural representations of parsers and printers"
**masaeedu**

"I suspect a "clean" way to do this sort of thing will eventually reveal within a profunctor encoding, but I haven't put much time into it yet :)" **ChrisPenner**

"Are there any bidirectional parser libraries ready to use?" **TheMatten**

"None that are good.
Well "good" is not the right word here
Basically the best current approach to bidirectional parsing in haskell does not have the ideal interface"
**chessai**

# Existing Implementations

## [codec](https://hackage.haskell.org/package/codec) Simple bidirectional serialization
Codec makes it simple to write composable bidirectional serializers with a consistent interface.

Pros
-

Cons
-

Features we would like to also have
-

## [lens](https://hackage.haskell.org/package/lens) Lenses, Folds and Traversals

Pros
-

Cons
-

Features we would like to also have
-

## [boomerang](https://hackage.haskell.org/package/boomerang) Library for invertible parsing and printing
Specify a single unified grammar which can be used for parsing and pretty-printing

Pros
-

Cons
-

Features we would like to also have
-

## [roundtrip](https://hackage.haskell.org/package/roundtrip) Bidirectional (de-)serialization
Roundtrip allows the definition of bidirectional (de-)serialization specifications. The specification language is based on the ideas described in the paper Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing by Tillmann Rendel and Klaus Ostermann, Haskell Symposium 2010.
This package does not provide concrete instances of the specification classes, see the packages roundtrip-string and roundtrip-xml instead.
The package contains slightly modified code from Tillmann Rendel's partial-isomorphisms and invertible-syntax packages (Copyright (c) 2010-11 University of Marburg).

Pros
-

Cons
-

Features we would like to also have
-

## [attoparsec](https://hackage.haskell.org/package/attoparsec) with [unparse-attoparsec](https://github.com/Lysxia/unparse-attoparsec)
This library provides an interface to build programs that can be interpreted both as parsers and as printers.

Pros
-

Cons
-

Features we would like to also have
-
