{-# LANGUAGE NoImplicitPrelude #-}

{-|
The "Data.Stream.Typed" module contains more detailed documenation.

This module simply imports functions from "Data.Stream.Typed" and modifies them so
inputs and outputs are always of type 'Data.Stream.Typed.UnknownStream', which this
module renames 'Stream' (yes, this clashes with 'Data.Stream.Typed.Stream' in
"Data.Stream.Typed").

Because of this, using this module more closely emulates how ordinary lists work,
but you miss some of the compile time information you can get using the \"typed\" module.
-}
module Data.Stream (
  Stream,
  ToStream,
  toStream,
  runTimeFoldableToStream,
  runTimeFoldableToStreamWithLength,
  unknownFoldableToStream,
  Element,
  empty,
  singleton,
  append,
  zip, zipWith,
  filter,
  concat,
  concatMap,
  replicate,
  iterate,
  repeat,
  cycle,
  null,
  unfoldr,
  safeLength, SafeLength(KnownSafeLength, UnknownSafeLength, InfiniteSafeLength),
  maybeHead,
  memotise
  )
where

import Prelude (
  (.),
  Bool,
  Int,
  Integral,
  Maybe,
  Foldable
  )

import Data.Stream.Typed (
  ToStream,
  Element,
  SafeLength(KnownSafeLength, UnknownSafeLength, InfiniteSafeLength),
  wrapUnknown,
  unfoldr,
  unknownFoldableToStream
  )

import qualified Data.Stream.Typed as T

type Stream a = T.UnknownStream a

wrap :: T.Stream l a -> Stream a
wrap = T.wrapUnknown

toStream :: (ToStream a) => a -> Stream (Element a)
toStream = wrap . T.toStream

empty :: Stream a
empty = wrap T.empty

singleton :: a -> Stream a
singleton = wrap . T.singleton

append :: Stream a -> Stream a -> Stream a
append = T.append

zip :: Stream a -> Stream b -> Stream (a, b)
zip = T.zip

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith = T.zipWith

filter :: (a -> Bool) -> Stream a -> Stream a
filter = T.filter

concat :: Stream (Stream a) -> Stream a
concat = T.concat

concatMap :: (a -> Stream b) -> Stream a -> Stream b
concatMap = T.concatMap

replicate :: (Integral b) => b -> a -> Stream a
replicate n x = wrap (T.replicate n x)

iterate :: (a -> a) -> a -> Stream a
iterate f x = wrap (T.iterate f x)

repeat :: a -> Stream a
repeat = wrap . repeat

cycle :: Stream a -> Stream a
cycle = wrap . cycle

safeLength :: Stream a -> SafeLength
safeLength = T.safeLength

maybeHead :: Stream a -> Maybe a
maybeHead = T.maybeHead

memotise :: Stream a -> Stream a
memotise = T.memotise

runTimeFoldableToStream :: (Foldable t) => t a -> Stream a
runTimeFoldableToStream = wrap . T.runTimeFoldableToStream

runTimeFoldableToStreamWithLength :: (Foldable t) => Int -> t a -> Stream a
runTimeFoldableToStreamWithLength n x = wrap (T.runTimeFoldableToStreamWithLength n x)

null :: Stream a -> Bool
null = T.null