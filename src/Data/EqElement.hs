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
import Data.MonoTraversable.Unprefixed ()
import Data.Sequences (IsSequence, singleton, splitWhen, fromList, isPrefixOf, drop, lengthIndex)
import Data.MonoTraversable.Unprefixed (null)
import Data.Monoid (mempty)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Search qualified as SS
import Data.ByteString.Lazy.Search qualified as SL
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

instance EqElement BS.ByteString where
  stripPrefix x y
    | x `isPrefixOf` y = Just (drop (lengthIndex x) y)
    | otherwise = Nothing
  splitElem sep s
    | null s = [mempty]
    | otherwise = BS.split sep s
  splitSeq sep s
    | null sep = (:) mempty $ List.map singleton $ BS.unpack s
    | null s = [mempty]
    | otherwise = SS.split sep s

instance EqElement BL.ByteString where
  stripPrefix x y
    | x `isPrefixOf` y = Just (drop (lengthIndex x) y)
    | otherwise = Nothing
  splitElem sep s
    | null s = [mempty]
    | otherwise = BL.split sep s
  splitSeq sep s
    | null sep = (:) mempty $ List.map singleton $ BL.unpack s
    | null s = [mempty]
    | otherwise = SL.split (BL.toStrict sep) s

instance EqElement T.Text where
  stripPrefix = T.stripPrefix
  splitSeq sep
    | null sep = (:) mempty . List.map singleton . T.unpack
    | otherwise = T.splitOn sep

instance Eq a => EqElement (Seq.Seq a)

