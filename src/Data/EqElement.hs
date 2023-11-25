-- | Hack until https://github.com/snoyberg/mono-traversable/issues/216 is resolved
{-# LANGUAGE NoImplicitPrelude #-}
module Data.EqElement
  ( EqElement(..)
  ) where

import Data.Bool (otherwise)
import Data.Eq (Eq, (==))
import Data.Function ((.), ($))
import Data.Functor (fmap)
import Data.Maybe (Maybe(Just,Nothing))
import Data.MonoTraversable (Element, otoList)
import Data.Sequences (IsSequence, singleton, splitWhen, fromList)

import Data.ByteString qualified as S
import Data.ByteString.Search qualified as SS
import Data.List qualified as List
import Data.List.Split qualified as List
import Data.Sequence qualified as Seq
import Data.Text qualified as T

class (IsSequence seq, Eq (Element seq)) => EqElement seq where
  stripPrefix :: seq -> seq -> Maybe seq
  stripPrefix x y = fmap fromList (otoList x `List.stripPrefix` otoList y)
  splitElem :: Element seq -> seq -> [seq]
  splitElem = splitWhen . (==)
  splitSeq :: seq -> seq -> [seq]
  splitSeq sep = List.map fromList . List.splitOn (otoList sep) . otoList

instance Eq a => EqElement [a] where
  stripPrefix = List.stripPrefix
  splitSeq = List.splitOn

instance EqElement S.ByteString where
  stripPrefix x y
    | x `S.isPrefixOf` y = Just (S.drop (S.length x) y)
    | otherwise = Nothing
  splitElem sep s
    | S.null s = [S.empty]
    | otherwise = S.split sep s
  splitSeq sep s
    | S.null sep = (:) S.empty $ List.map singleton $ S.unpack s
    | S.null s = [S.empty]
    | otherwise = SS.split sep s

instance EqElement T.Text where
  stripPrefix = T.stripPrefix
  splitSeq sep
    | T.null sep = (:) T.empty . List.map singleton . T.unpack
    | otherwise = T.splitOn sep

instance Eq a => EqElement (Seq.Seq a)

