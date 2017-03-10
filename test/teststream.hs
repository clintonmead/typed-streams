{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Main where

import Data.Bits ((.&.))

import GHC.TypeLits (
  Nat, natVal, KnownNat,
  type (+), type (-), type (*), type (<=), type (<=?)
  )

import Data.Proxy (Proxy)

import Prelude (
  Functor, fmap, (<$),
  Maybe(Just, Nothing),
  Int, Integer, fromInteger, Integral,
  Char,
  Either(Left, Right),
  (.),
  const,
  (+), (-), (*), (>=), (==), (>>), (/=),
  ($!), seq,
  undefined,
  error,
  Bool (True, False),
  (>>=),
  return,
  min,
  snd,
  id,
  (&&),
  Integral, fromIntegral
  )

import Control.Applicative (
  Applicative, pure, (<*>),
  Alternative, empty, (<|>)
  )

import Control.Monad (
  Monad, (>>=), return,
  MonadPlus
  )

import Data.Monoid (
  Monoid, mempty, mappend, mconcat
  )


import qualified Prelude

import Data.Foldable (
  Foldable, foldr, length,
  foldl', toList,
  null, all
  )

import Control.Arrow (first, second)

import Data.Proxy (Proxy(Proxy))
import Data.Maybe (catMaybes)

import GHC.Exts (
  Constraint,
  IsList, fromList, fromListN
  )

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Vector

import qualified GHC.Exts

import Data.Type.Bool (type If)

import Control.Monad (foldM_)

import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as Unboxed

import Data.MonoTraversable.WrapMonoFoldable (WrappedMonoFoldable(WrappedMonoFoldable))

import Data.Semigroup (
  Semigroup, (<>), stimes
  )

import GHC.Exts (Item)

data Length = Unknown | Infinite | Known KnownType
data KnownType = RunTimeLength | CompileTimeLength CompileTimeLengthType
--data KnownType = RunTimeLength Nat | CompileTimeLength CompileTimeLengthType

data CompileTimeLengthType = NatLength Nat | Zero

type RunTime = Known RunTimeLength
--type RunTime n = Known (RunTimeLength n)
type CompileTime n = Known (CompileTimeLength (NatLength n))
type Empty = Known (CompileTimeLength Zero)

type InfiniteStream a = Stream Infinite a
type UnknownStream a = Stream Unknown a
type RunTimeStream a = Stream RunTime a
type CompileTimeStream n a = Stream (CompileTime n) a
type EmptyStream a = Stream Empty a
{-
type family RunTimeKnownLength (l :: KnownType) = (r :: Nat) where
  RunTimeKnownLength (RunTimeLength n1) = n1
  RunTimeKnownLength (CompileTimeLength _) = 1

type AddRunTimeKnownLength (l1 :: KnownType) (l2 :: KnownType) = RunTimeKnownLength l1 + RunTimeKnownLength l2

type family RunTimeLength (l :: Length) = (r :: Nat) where
  RunTimeLength (Known kl) = RunTimeKnownLength kl
  RunTimeLength _ = 1

type AddRunTimeLength l1 l2 = RunTimeLength l1 + RunTimeLength l2
-}
data Stream (x :: Length) a where
  EmptyStream :: EmptyStream a
  SingletonStream :: a -> CompileTimeStream 1 a

  CompileTimeSingleStream :: (KnownNat n) => (s -> (a,s)) -> s -> CompileTimeStream n a
  RunTimeSingleStream :: Int -> (s -> (a,s)) -> s -> RunTimeStream a
  UnknownSingleStream :: (s -> Maybe (a,s)) -> s -> UnknownStream a
  InfiniteSingleStream :: (s -> (a,s)) -> s -> InfiniteStream a

  CompileTimeConstantStream :: (KnownNat n) => a -> CompileTimeStream n a
  RunTimeConstantStream :: Int -> a -> RunTimeStream a
  UnknownConstantStream :: (s -> Maybe s) -> s -> a -> UnknownStream a
  InfiniteConstantStream :: a -> InfiniteStream a

  CompileTimeAppendStream :: (KnownNat n1, KnownNat n2) => CompileTimeStream n1 a -> CompileTimeStream n2 a -> CompileTimeStream (n1 + n2) a
  RunTimeAppendStream :: Stream (Known l1) a -> Stream (Known l2) a -> RunTimeStream a
  UnknownAppendStream :: Stream l1 a -> Stream l2 a -> UnknownStream a
  InfiniteAppendStream :: Stream l1 a -> InfiniteStream a -> InfiniteStream a

  UnknownUntypedStream :: Stream l a -> UnknownStream a
  RunTimeUntypedStream :: Stream (Known l) a -> RunTimeStream a

  CompileTimeFoldableStream :: (KnownNat n, Foldable t) => (b -> a) -> t b -> CompileTimeStream n a
  RunTimeFoldableStream :: (Foldable t) => Int -> (b -> a) -> t b -> RunTimeStream a
  UnknownFoldableStream :: (Foldable t) => (b -> Maybe a) -> t b -> UnknownStream a
  InfiniteFoldableStream :: (Foldable t) => (b -> a) -> t b -> InfiniteStream a

  CompileTimeZipStream :: (KnownNat n) => (a -> b -> c) -> Stream l1 a -> Stream l2 b ->  CompileTimeStream n c
  RunTimeZipStream :: (a -> b -> c) -> Stream l1 a -> Stream l2 b -> RunTimeStream c
  UnknownZipStream :: (a -> b -> Maybe c) -> Stream l1 a -> Stream l2 b -> UnknownStream c
  InfiniteZipStream :: (a -> b -> c) -> InfiniteStream a -> InfiniteStream b -> InfiniteStream c

  CompileTimeConcatStream :: (KnownNat n1, KnownNat n2) => CompileTimeStream n1 (CompileTimeStream n2 a) -> CompileTimeStream (n1 * n2) a
  RunTimeConcatStream :: Int -> Stream (Known l1) (Stream (Known l2) a) -> RunTimeStream a
  UnknownConcatStream :: Stream l1 (Stream l2 a) -> UnknownStream a
  InfiniteConcatStream :: InfiniteStream (Stream l2 a) -> InfiniteStream a

  CompileTimeLazyMemotisedStream :: (KnownNat n) => Vector a -> CompileTimeStream n a
  RunTimeLazyMemotisedStream :: Vector a -> RunTimeStream a
  UnknownLazyMemotisedStream :: [a] -> UnknownStream a
  InfiniteLazyMemotisedStream :: [a] -> InfiniteStream a

  CompileTimeStrictMemotisedStream :: (KnownNat n, Unbox a) => Unboxed.Vector a -> CompileTimeStream n a
  RunTimeStrictMemotisedStream :: (Unbox a) => Unboxed.Vector a -> RunTimeStream a

pattern EmptyPattern :: () => (l ~ Empty) => Stream l a
pattern EmptyPattern =  EmptyStream

pattern SingletonPattern :: () => (l ~ CompileTime n, KnownNat n) => Stream l a
pattern SingletonPattern <- SingletonStream _

pattern CompileTimeSinglePattern :: () => (l ~ CompileTime n, KnownNat n) => Stream l a
pattern CompileTimeSinglePattern <- CompileTimeSingleStream _ _

pattern RunTimeSinglePattern :: () => (l ~ RunTime) => Stream l a
pattern RunTimeSinglePattern <- RunTimeSingleStream _ _ _

pattern UnknownSinglePattern :: () => (l ~ Unknown) => Stream l a
pattern UnknownSinglePattern <- UnknownSingleStream _ _

pattern InfiniteSinglePattern :: () => (l ~ Infinite) => Stream l a
pattern InfiniteSinglePattern <- InfiniteSingleStream _ _

pattern CompileTimeConstantPattern :: () => (l ~ CompileTime n, KnownNat n) => Stream l a
pattern CompileTimeConstantPattern <- CompileTimeConstantStream _

pattern RunTimeConstantPattern :: () => (l ~ RunTime) => Stream l a
pattern RunTimeConstantPattern <- RunTimeConstantStream _ _

pattern UnknownConstantPattern :: () => (l ~ Unknown) => Stream l a
pattern UnknownConstantPattern <- UnknownConstantStream _ _ _

pattern InfiniteConstantPattern :: () => (l ~ Infinite) => Stream l a
pattern InfiniteConstantPattern <- InfiniteConstantStream _

pattern CompileTimeAppendPattern :: () => (l ~ CompileTime n, KnownNat n) => Stream l a
pattern CompileTimeAppendPattern <- CompileTimeAppendStream _ _

pattern RunTimeAppendPattern :: () => (l ~ RunTime) => Stream l a
pattern RunTimeAppendPattern <- RunTimeAppendStream _ _

pattern UnknownAppendPattern :: () => (l ~ Unknown) => Stream l a
pattern UnknownAppendPattern <- UnknownAppendStream _ _

pattern InfiniteAppendPattern :: () => (l ~ Infinite) => Stream l a
pattern InfiniteAppendPattern <- InfiniteAppendStream _ _

pattern CompileTimeZipPattern :: () => (l ~ CompileTime n, KnownNat n) => Stream l a
pattern CompileTimeZipPattern <- CompileTimeZipStream _ _ _

pattern RunTimeZipPattern :: () => (l ~ RunTime) => Stream l a
pattern RunTimeZipPattern <- RunTimeZipStream _ _ _

pattern UnknownZipPattern :: () => (l ~ Unknown) => Stream l a
pattern UnknownZipPattern <- UnknownZipStream _ _ _

pattern InfiniteZipPattern :: () => (l ~ Infinite) => Stream l a
pattern InfiniteZipPattern <- InfiniteZipStream _ _ _

pattern UnknownUntypedPattern :: () => (l ~ Unknown) => Stream l a
pattern UnknownUntypedPattern <- UnknownUntypedStream _

pattern RunTimeUntypedPattern :: () => (l ~ RunTime) => Stream l a
pattern RunTimeUntypedPattern <- RunTimeUntypedStream _

pattern CompileTimeFoldablePattern :: () => (l ~ CompileTime n, KnownNat n) => Stream l a
pattern CompileTimeFoldablePattern <- CompileTimeFoldableStream _ _

pattern RunTimeFoldablePattern :: () => (l ~ RunTime) => Stream l a
pattern RunTimeFoldablePattern <- RunTimeFoldableStream _ _ _

pattern UnknownFoldablePattern :: () => (l ~ Unknown) => Stream l a
pattern UnknownFoldablePattern <- UnknownFoldableStream _ _

pattern InfiniteFoldablePattern :: () => (l ~ Infinite) => Stream l a
pattern InfiniteFoldablePattern <- InfiniteFoldableStream _ _

pattern CompileTimeConcatPattern :: () => (l ~ CompileTime n, KnownNat n) => Stream l a
pattern CompileTimeConcatPattern <- CompileTimeConcatStream _

pattern RunTimeConcatPattern :: () => (l ~ RunTime) => Stream l a
pattern RunTimeConcatPattern <- RunTimeConcatStream _ _

pattern UnknownConcatPattern :: () => (l ~ Unknown) => Stream l a
pattern UnknownConcatPattern <- UnknownConcatStream _

pattern InfiniteConcatPattern :: () => (l ~ Infinite) => Stream l a
pattern InfiniteConcatPattern <- InfiniteConcatStream _

pattern CompileTimeLazyMemotisedPattern :: () => (l ~ CompileTime n, KnownNat n) => Stream l a
pattern CompileTimeLazyMemotisedPattern <- CompileTimeLazyMemotisedStream _

pattern RunTimeLazyMemotisedPattern :: () => (l ~ RunTime) => Stream l a
pattern RunTimeLazyMemotisedPattern <- RunTimeLazyMemotisedStream _

pattern UnknownLazyMemotisedPattern :: () => (l ~ Unknown) => Stream l a
pattern UnknownLazyMemotisedPattern <- UnknownLazyMemotisedStream _

pattern InfiniteLazyMemotisedPattern :: () => (l ~ Infinite) => Stream l a
pattern InfiniteLazyMemotisedPattern <- InfiniteLazyMemotisedStream _

pattern CompileTimeStrictMemotisedPattern :: () => (l ~ CompileTime n, KnownNat n) => Stream l a
pattern CompileTimeStrictMemotisedPattern <- CompileTimeStrictMemotisedStream _

pattern RunTimeStrictMemotisedPattern :: () => (l ~ RunTime) => Stream l a
pattern RunTimeStrictMemotisedPattern <- RunTimeStrictMemotisedStream _


data StepResult a s = StepResult (a,s) | NewState (State a) | End

data State a where
  State :: (s -> StepResult a s) -> s -> State a

step :: State a -> Maybe (a, State a)
step (State sf s) = case sf s of
  StepResult (a,s) -> Just (a, State sf s)
  NewState ns -> step ns
  End -> Nothing


data StreamType (x :: Length) where
  InfiniteStreamType :: StreamType Infinite
  UnknownStreamType :: StreamType Unknown
  RunTimeStreamType :: StreamType RunTime
  CompileTimeStreamType :: (KnownNat n) => StreamType (CompileTime n)
  EmptyStreamType :: StreamType Empty

replicate :: (Integral b) => b -> a -> RunTimeStream a
replicate n = RunTimeConstantStream (fromIntegral n)

type family LengthT a = (r :: Length)

class ToStream a where
  toStream :: a -> Stream (LengthT a) (Item a)

type instance LengthT [a] = Unknown
instance ToStream [a] where
  toStream x = UnknownFoldableStream pure x

getStreamType :: forall l a. Stream l a -> StreamType l
getStreamType x = case x of
  InfiniteSinglePattern -> InfiniteStreamType
  InfiniteAppendPattern -> InfiniteStreamType
  InfiniteFoldablePattern -> InfiniteStreamType
  InfiniteConstantPattern -> InfiniteStreamType
  InfiniteZipPattern -> InfiniteStreamType
  InfiniteConcatPattern -> InfiniteStreamType
  InfiniteLazyMemotisedPattern -> InfiniteStreamType
  UnknownSinglePattern -> UnknownStreamType
  UnknownAppendPattern -> UnknownStreamType
  UnknownFoldablePattern -> UnknownStreamType
  UnknownUntypedPattern -> UnknownStreamType
  UnknownZipPattern -> UnknownStreamType
  UnknownConstantPattern -> UnknownStreamType
  UnknownConcatPattern -> UnknownStreamType
  UnknownLazyMemotisedPattern -> UnknownStreamType
  RunTimeSinglePattern -> RunTimeStreamType
  RunTimeAppendPattern -> RunTimeStreamType
  RunTimeFoldablePattern -> RunTimeStreamType
  RunTimeConstantPattern -> RunTimeStreamType
  RunTimeUntypedPattern -> RunTimeStreamType
  RunTimeZipPattern -> RunTimeStreamType
  RunTimeConcatPattern -> RunTimeStreamType
  RunTimeLazyMemotisedPattern -> RunTimeStreamType
  RunTimeStrictMemotisedPattern -> RunTimeStreamType
  CompileTimeSinglePattern -> CompileTimeStreamType
  CompileTimeAppendPattern -> CompileTimeStreamType
  CompileTimeFoldablePattern -> CompileTimeStreamType
  CompileTimeConstantPattern -> CompileTimeStreamType
  CompileTimeZipPattern -> CompileTimeStreamType
  CompileTimeConcatPattern -> CompileTimeStreamType
  CompileTimeLazyMemotisedPattern -> CompileTimeStreamType
  CompileTimeStrictMemotisedPattern -> CompileTimeStreamType
  SingletonPattern -> CompileTimeStreamType
  EmptyPattern -> EmptyStreamType
  _ -> patternSynonymCatchAll

patternSynonymCatchAll :: a
patternSynonymCatchAll = error "Annoying catch all due to exhaustiveness checking not working for pattern synonyms. You should never reach here."

type family AppendLength (a :: Length) (b :: Length) where
  AppendLength _ Infinite = Infinite
  AppendLength Infinite _ = Infinite
  AppendLength Empty y = y
  AppendLength x Empty = x
  AppendLength (CompileTime n1) (CompileTime n2) = CompileTime (n1 + n2)
  AppendLength (Known l1) (Known l2) = RunTime
  AppendLength _ _ = Unknown

data RunTimeWrapper a where
  RunTimeWrapper :: Stream (Known l) a -> RunTimeWrapper a

data UnknownWrapper a where
  UnknownWrapper :: Stream l a -> UnknownWrapper a


append :: forall l1 l2 a. Stream l1 a -> Stream l2 a -> Stream (AppendLength l1 l2) a
append x y = go (getStreamType x) (getStreamType y) where
  go :: StreamType l1 -> StreamType l2 -> Stream (AppendLength l1 l2) a
  go InfiniteStreamType _ = x
  go EmptyStreamType _ = y
  go _ EmptyStreamType = x
  go _ InfiniteStreamType = mkInfiniteAppendStream x y
  go CompileTimeStreamType CompileTimeStreamType = mkCompileTimeAppendStream x y
  go CompileTimeStreamType RunTimeStreamType = mkRunTimeAppendStream x y
  go RunTimeStreamType CompileTimeStreamType = mkRunTimeAppendStream x y
  go RunTimeStreamType RunTimeStreamType = mkRunTimeAppendStream x y
  go CompileTimeStreamType UnknownStreamType = mkUnknownAppendStream x y
  go RunTimeStreamType UnknownStreamType = mkUnknownAppendStream x y
  go UnknownStreamType CompileTimeStreamType = mkUnknownAppendStream x y
  go UnknownStreamType RunTimeStreamType = mkUnknownAppendStream x y
  go UnknownStreamType UnknownStreamType = mkUnknownAppendStream x y


  mkCompileTimeAppendStream :: (KnownNat n1, KnownNat n2) => CompileTimeStream n1 a -> CompileTimeStream n2 a -> CompileTimeStream (n1 + n2) a
  mkCompileTimeAppendStream = CompileTimeAppendStream


  mkUnknownAppendStream :: Stream l1 a -> Stream l2 a -> UnknownStream a
  mkUnknownAppendStream x y =
    case (mkUnknownWrapper x, mkUnknownWrapper y) of
      (UnknownWrapper x', UnknownWrapper y') -> UnknownAppendStream x' y'

  mkInfiniteAppendStream :: Stream l1 a -> InfiniteStream a -> InfiniteStream a
  mkInfiniteAppendStream = InfiniteAppendStream

  mkRunTimeAppendStream :: Stream (Known l1') a -> Stream (Known l2') a -> RunTimeStream a
  mkRunTimeAppendStream x y =
    case (mkRunTimeWrapper x, mkRunTimeWrapper y) of
      (RunTimeWrapper x', RunTimeWrapper y') -> RunTimeAppendStream x' y'

mkUnknownWrapper :: Stream l a -> UnknownWrapper a
mkUnknownWrapper (RunTimeUntypedStream x) = UnknownWrapper x
mkUnknownWrapper (UnknownUntypedStream x) = UnknownWrapper x
mkUnknownWrapper x = UnknownWrapper x

mkRunTimeWrapper :: Stream (Known l) a -> RunTimeWrapper a
mkRunTimeWrapper (RunTimeUntypedStream x) = RunTimeWrapper x
mkRunTimeWrapper x = RunTimeWrapper x

type Min (n1 :: Nat) (n2 :: Nat) = If (n1 <=? n2) n1 n2

type family ZipLength (a :: Length) (b :: Length) where
  ZipLength Empty _ = Empty
  ZipLength _ Empty = Empty
  ZipLength x Infinite = x
  ZipLength Infinite y = y
  ZipLength (CompileTime n1) (CompileTime n2) = CompileTime (Min n1 n2)
  ZipLength (Known l1) (Known l2) = RunTime
  ZipLength _ _ = Unknown

data BooleanTest (a :: Bool) where
  BooleanTestTrue :: BooleanTest True
  BooleanTestFalse :: BooleanTest False

zip :: Stream l1 a -> Stream l2 b -> Stream (ZipLength l1 l2) (a,b)
zip = zipWith (,)

zipWith :: forall l1 l2 a b c. (a -> b -> c) -> Stream l1 a -> Stream l2 b -> Stream (ZipLength l1 l2) c
zipWith f x y = go (getStreamType x) (getStreamType y) where
  go :: StreamType l1 -> StreamType l2 -> Stream (ZipLength l1 l2) c
  go EmptyStreamType _ = EmptyStream
  go _ EmptyStreamType = EmptyStream

  go CompileTimeStreamType InfiniteStreamType = mkCompileTime
  go InfiniteStreamType CompileTimeStreamType = mkCompileTime
  go RunTimeStreamType InfiniteStreamType = mkRunTime
  go InfiniteStreamType RunTimeStreamType = mkRunTime
  go UnknownStreamType InfiniteStreamType = mkUnknown
  go InfiniteStreamType UnknownStreamType = mkUnknown
  go InfiniteStreamType InfiniteStreamType = mkInfinite

  go CompileTimeStreamType CompileTimeStreamType = go' where
    go' :: forall n1 n2. (l1 ~ CompileTime n1, l2 ~ CompileTime n2) => Stream (CompileTime (Min n1 n2)) c
    go' =
      case (undefined :: (BooleanTest (n1 <=? n2))) of
        BooleanTestTrue -> mkCompileTime
        BooleanTestFalse -> mkCompileTime

  go CompileTimeStreamType RunTimeStreamType = mkRunTime
  go RunTimeStreamType CompileTimeStreamType = mkRunTime
  go RunTimeStreamType RunTimeStreamType = mkRunTime
  go CompileTimeStreamType UnknownStreamType = mkUnknown
  go RunTimeStreamType UnknownStreamType = mkUnknown
  go UnknownStreamType UnknownStreamType = mkUnknown
  go UnknownStreamType CompileTimeStreamType = mkUnknown
  go UnknownStreamType RunTimeStreamType = mkUnknown


  mkCompileTime :: (ZipLength l1 l2 ~ CompileTime n, KnownNat n) => CompileTimeStream n c
  mkCompileTime = CompileTimeZipStream f x y

  mkRunTime :: (ZipLength l1 l2 ~ RunTime) => RunTimeStream c
  mkRunTime =
    case (mkUnknownWrapper x, mkUnknownWrapper y) of
      (UnknownWrapper x', UnknownWrapper y') -> RunTimeZipStream f x' y'

  mkUnknown :: (ZipLength l1 l2 ~ Unknown) => UnknownStream c
  mkUnknown =
    case (mkUnknownWrapper x, mkUnknownWrapper y) of
      (UnknownWrapper x', UnknownWrapper y') -> UnknownZipStream (pure `compose2` f) x' y'

  mkInfinite :: (l1 ~ Infinite, l2 ~ Infinite) => InfiniteStream c
  mkInfinite = InfiniteZipStream f x y

foldableToVector :: Foldable t => t a -> Vector a
foldableToVector l =
  Vector.create
  (
    do
      v <- Vector.new (length l)
      let f i x = Vector.write v i x >> return (i+1)
      foldM_ f 0 l
      return v
  )

memotise :: Stream l a -> Stream l a
memotise x = case getStreamType x of
  InfiniteStreamType -> case x of
    InfiniteLazyMemotisedStream _ -> x
    _ -> InfiniteLazyMemotisedStream (toList x)
  UnknownStreamType -> case x of
    UnknownLazyMemotisedStream _ -> x
    UnknownUntypedStream x -> wrap (memotise x)
    _ -> UnknownLazyMemotisedStream (toList x)
  RunTimeStreamType -> case x of
    RunTimeLazyMemotisedStream _ -> x
    RunTimeStrictMemotisedStream _ -> x
    RunTimeUntypedStream x -> wrapRunTime (memotise x)
    _ -> RunTimeLazyMemotisedStream (foldableToVector x)
  CompileTimeStreamType -> CompileTimeLazyMemotisedStream (foldableToVector x)
  EmptyStreamType -> EmptyStream


filter :: forall l a. (a -> Bool) -> Stream l a -> UnknownStream a
filter f x = go x where
  go :: forall l. Stream l a -> UnknownStream a
  go EmptyStream = emptyStream
  go (SingletonStream e) = filterConstant e

  go (CompileTimeConstantStream e) = filterConstant e
  go (RunTimeConstantStream _ e) = filterConstant e
  go (UnknownConstantStream _ _ e) = filterConstant e
  go (InfiniteConstantStream e) = filterConstant e

  go (CompileTimeSingleStream sf s) = mkUnknownSingleStreamFromLimited (length x) sf s
  go (RunTimeSingleStream n sf s) = mkUnknownSingleStreamFromLimited n sf s
  go (UnknownSingleStream sf s) = UnknownSingleStream h s where
    h s = case (sf s) of
      Nothing -> Nothing
      result_plus_state@(Just (result, new_state)) ->
        case (f result) of
          True -> result_plus_state
          False -> h new_state
  go (InfiniteSingleStream sf s) = wrap (InfiniteSingleStream h s) where
    h s =
      let
        result_plus_state@(result, new_state) = sf s
      in
        case (f result) of
          True -> result_plus_state
          False -> h new_state

  go (CompileTimeAppendStream x y) = filterAppend x y
  go (RunTimeAppendStream x y) = filterAppend x y
  go (UnknownAppendStream x y) = filterAppend x y
  go (InfiniteAppendStream x y) = filterAppend x y

  go (UnknownUntypedStream x) = wrap (go x)
  go (RunTimeUntypedStream x) = wrap (go x)

  go (CompileTimeFoldableStream g x) = filterFoldable g x
  go (RunTimeFoldableStream _ g x) = filterFoldable g x
  go (UnknownFoldableStream g x) = filterFoldableMaybe g x
  go (InfiniteFoldableStream g x) = filterFoldable g x

  go (CompileTimeZipStream g x y) = filterZip g x y
  go (RunTimeZipStream g x y) = filterZip g x y
  go (UnknownZipStream g x y) = filterZipMaybe g x y
  go (InfiniteZipStream g x y) = filterZip g x y

  go (CompileTimeConcatStream x) = filterConcat x
  go (RunTimeConcatStream _ x) = filterConcat x
  go (UnknownConcatStream x) = filterConcat x
  go (InfiniteConcatStream x) = filterConcat x

  go (CompileTimeLazyMemotisedStream x) = filterFoldable id x
  go (RunTimeLazyMemotisedStream x) = filterFoldable id x
  go (UnknownLazyMemotisedStream x) = filterFoldable id x
  go (InfiniteLazyMemotisedStream x) = filterFoldable id x

  go (CompileTimeStrictMemotisedStream x) = filterFoldable id (WrappedMonoFoldable x)
  go (RunTimeStrictMemotisedStream x) = filterFoldable id (WrappedMonoFoldable x)

  filterConcat :: forall l1 l2. Stream l1 (Stream l2 a) -> UnknownStream a
  filterConcat x = UnknownConcatStream (fmap go x)

  filterZip :: forall b1 b2 l1 l2. (b1 -> b2 -> a) -> Stream l1 b1 -> Stream l2 b2 -> UnknownStream a
  filterZip g = UnknownZipStream (filterMaybe `compose2` g)

  filterZipMaybe :: forall b1 b2 l1 l2. (b1 -> b2 -> Maybe a) -> Stream l1 b1 -> Stream l2 b2 -> UnknownStream a
  filterZipMaybe g = UnknownZipStream (\x y -> g x y >>= filterMaybe)

  filterFoldableMaybe :: forall t b. Foldable t => (b -> Maybe a) -> t b -> Stream Unknown a
  filterFoldableMaybe g = UnknownFoldableStream (\x -> g x >>= filterMaybe)

  filterFoldable :: forall t b. Foldable t => (b -> a) -> t b -> Stream Unknown a
  filterFoldable g = UnknownFoldableStream (filterMaybe . g)

  filterMaybe :: a -> Maybe a
  filterMaybe x = if (f x) then Just x else Nothing

  filterAppend :: forall l1 l2. Stream l1 a -> Stream l2 a -> Stream Unknown a
  filterAppend x y = UnknownAppendStream (go x) (go y)

  mkUnknownSingleStreamFromLimited :: forall s. Int -> (s -> (a,s)) -> s -> UnknownStream a
  mkUnknownSingleStreamFromLimited n sf s = UnknownSingleStream (h sf) (s, n) where
    h :: (s -> (a,s)) -> (s, Int) -> Maybe (a, (s, Int))
    h sf (s, n) = go s n where
      go _ 0 = Nothing
      go s n =
        let
          n_minus_1 = n - 1
          (result, new_state) = sf s
        in
          case (f result) of
            True -> Just (result, (new_state, n_minus_1))
            False -> go new_state n_minus_1


  filterConstant e = if f e then wrap x else emptyStream
  emptyStream = UnknownUntypedStream EmptyStream


instance Functor (Stream l) where
  fmap :: forall a b l. (a -> b) -> Stream l a -> Stream l b
  fmap f = go where
    go :: forall l. Stream l a -> Stream l b
    go (InfiniteSingleStream sf s) = InfiniteSingleStream ((first f) . sf) s
    go (InfiniteAppendStream x y) = InfiniteAppendStream (go x) (go y)

    go (UnknownSingleStream sf s) = UnknownSingleStream ((fmap (first f)) . sf) s
    go (UnknownAppendStream x y) = UnknownAppendStream (go x) (go y)
    go (UnknownUntypedStream x) = UnknownUntypedStream (go x)

    go (RunTimeSingleStream n sf s) = RunTimeSingleStream n ((first f) . sf) s
    go (RunTimeAppendStream x y) = RunTimeAppendStream (go x) (go y)
    go (RunTimeUntypedStream x) = RunTimeUntypedStream (go x)

    go (CompileTimeSingleStream sf s) = CompileTimeSingleStream ((first f) . sf) s
    go (CompileTimeAppendStream x y) = CompileTimeAppendStream (go x) (go y)
    go (SingletonStream x) = SingletonStream (f x)
    go EmptyStream = EmptyStream

    go (CompileTimeConstantStream x) = CompileTimeConstantStream (f x)
    go (RunTimeConstantStream n x) = RunTimeConstantStream n (f x)
    go (UnknownConstantStream sf s x) = UnknownConstantStream sf s (f x)
    go (InfiniteConstantStream x) = InfiniteConstantStream (f x)

    go (CompileTimeFoldableStream g x) = CompileTimeFoldableStream (f . g) x
    go (RunTimeFoldableStream n g x) = RunTimeFoldableStream n (f . g) x
    go (UnknownFoldableStream g x) = UnknownFoldableStream (fmap f . g) x
    go (InfiniteFoldableStream g x) = InfiniteFoldableStream (f . g) x

    go (CompileTimeZipStream g x y) = CompileTimeZipStream (f `compose2` g) x y
    go (RunTimeZipStream g x y) = RunTimeZipStream (f `compose2` g) x y
    go (UnknownZipStream g x y) = UnknownZipStream (fmap f `compose2` g) x y
    go (InfiniteZipStream g x y) = InfiniteZipStream (f `compose2` g) x y

    go (CompileTimeConcatStream l) = CompileTimeConcatStream (fmap go l)
    go (RunTimeConcatStream n l) = RunTimeConcatStream n (fmap go l)
    go (UnknownConcatStream l) = UnknownConcatStream (fmap go l)
    go (InfiniteConcatStream l) = InfiniteConcatStream (fmap go l)

    go (CompileTimeLazyMemotisedStream x) = CompileTimeFoldableStream f x
    go (RunTimeLazyMemotisedStream x) = RunTimeFoldableStream (length x) f x
    go (UnknownLazyMemotisedStream x) = UnknownFoldableStream (pure . f) x
    go (InfiniteLazyMemotisedStream x) = InfiniteFoldableStream f x

    go (CompileTimeStrictMemotisedStream x) = CompileTimeFoldableStream f (WrappedMonoFoldable x)
    go (RunTimeStrictMemotisedStream x) = RunTimeFoldableStream (Unboxed.length x) f (WrappedMonoFoldable x)

  (<$) :: forall l a b. a -> Stream l b -> Stream l a
  (<$) e = go where
    go :: forall l. Stream l b -> Stream l a
    go x = case (getStreamType x) of
      InfiniteStreamType -> InfiniteConstantStream e
      UnknownStreamType -> case x of
        (UnknownAppendStream x y) -> UnknownAppendStream (go x) (go y)
        (UnknownUntypedStream x) -> wrap (go x)
        (UnknownConstantStream sf s _) -> UnknownConstantStream sf s e
        (UnknownSingleStream sf s) -> UnknownConstantStream ((fmap snd) . sf) s e
        (UnknownFoldableStream f l) -> UnknownFoldableStream (fmap (const e) . f) l
        (UnknownZipStream f x y) -> UnknownZipStream (fmap (const e) `compose2` f) x y
        (UnknownConcatStream x) -> UnknownConcatStream (fmap go x)
        (UnknownLazyMemotisedStream l) -> UnknownFoldableStream (pure . (const e)) l
      RunTimeStreamType -> case x of
        (RunTimeConcatStream n x) -> RunTimeConcatStream n (fmap go x)
        (RunTimeAppendStream x y) -> RunTimeAppendStream (go x) (go y)
        _ -> RunTimeConstantStream (length x) e
      CompileTimeStreamType -> CompileTimeConstantStream e
      EmptyStreamType -> EmptyStream


type family SafeHead (l :: Length) = (f :: Bool) where
  SafeHead Infinite = True
  SafeHead (CompileTime _) = True
  SafeHead _ = False


unsafeHead :: Foldable t => t a -> a
unsafeHead = foldr const (error "Empty Foldable")

safeHead :: (SafeHead l ~ True) => Stream l a -> a
safeHead = unsafeHead

maybeHead :: Stream l a -> Maybe a
maybeHead x = case getStreamType x of
  InfiniteStreamType -> Just (safeHead x)
  UnknownStreamType -> foldr (\e _ -> Just e) Nothing x
  RunTimeStreamType -> foldr (\e _ -> Just e) Nothing x
  CompileTimeStreamType -> Just (safeHead x)
  EmptyStreamType -> Nothing


class IsLengthType (l :: Length) where
  getStreamTypeFromProxy :: Proxy l -> StreamType l

instance IsLengthType Empty where
  getStreamTypeFromProxy _ = EmptyStreamType

instance (KnownNat n) => IsLengthType (CompileTime n) where
  getStreamTypeFromProxy _ = CompileTimeStreamType

instance IsLengthType RunTime where
  getStreamTypeFromProxy _ = RunTimeStreamType

instance IsLengthType Unknown where
  getStreamTypeFromProxy _ = UnknownStreamType

instance IsLengthType Infinite where
  getStreamTypeFromProxy _ = InfiniteStreamType

type family ConcatLength (l1 :: Length) (l2 :: Length) where
  ConcatLength Empty _ = Empty
  ConcatLength _ Empty = Empty
  ConcatLength Infinite Infinite = Infinite
  ConcatLength Infinite (CompileTime n1) = Infinite
  ConcatLength (CompileTime n1) Infinite = Infinite
  ConcatLength Infinite _ = Unknown
  ConcatLength _ Infinite = Unknown
  ConcatLength Unknown _ = Unknown
  ConcatLength _ Unknown = Unknown
  ConcatLength RunTime _ = RunTime
  ConcatLength _ RunTime = RunTime
  ConcatLength (CompileTime n1) (CompileTime n2) = CompileTime (n1 * n2)


mixedConcat :: forall l1 l2 a. IsLengthType l2 => Stream l1 (Stream l2 a) -> Stream (ConcatLength l1 l2) a
mixedConcat x = case (getStreamType x, getStreamTypeFromProxy (undefined :: Proxy l2)) of
  (EmptyStreamType, EmptyStreamType) -> EmptyStream
  (EmptyStreamType, CompileTimeStreamType) -> EmptyStream
  (EmptyStreamType, RunTimeStreamType) -> EmptyStream
  (EmptyStreamType, UnknownStreamType) -> EmptyStream
  (EmptyStreamType, InfiniteStreamType) -> EmptyStream
  (CompileTimeStreamType, EmptyStreamType) -> EmptyStream
  (RunTimeStreamType, EmptyStreamType) -> EmptyStream
  (UnknownStreamType, EmptyStreamType) -> EmptyStream
  (InfiniteStreamType, EmptyStreamType) -> EmptyStream
  (InfiniteStreamType, InfiniteStreamType) -> safeHead x
  (CompileTimeStreamType, InfiniteStreamType) -> safeHead x
  (InfiniteStreamType, CompileTimeStreamType) -> InfiniteConcatStream x
  (InfiniteStreamType, UnknownStreamType) -> UnknownConcatStream x
  (InfiniteStreamType, RunTimeStreamType) -> UnknownConcatStream x
  (UnknownStreamType, InfiniteStreamType) -> UnknownConcatStream x
  (RunTimeStreamType, InfiniteStreamType) -> UnknownConcatStream x
  (UnknownStreamType, UnknownStreamType) -> UnknownConcatStream x
  (UnknownStreamType, CompileTimeStreamType) -> UnknownConcatStream x
  (UnknownStreamType, RunTimeStreamType) -> UnknownConcatStream x
  (CompileTimeStreamType, UnknownStreamType) -> UnknownConcatStream x
  (RunTimeStreamType, UnknownStreamType) -> UnknownConcatStream x
  (RunTimeStreamType, RunTimeStreamType) -> RunTimeConcatStream (foldLength x) x
  (RunTimeStreamType, CompileTimeStreamType) -> let n1 = length x in if n1 /= 0 then RunTimeConcatStream (n1 * length (unsafeHead x)) x else RunTimeUntypedStream EmptyStream
  (CompileTimeStreamType, RunTimeStreamType) -> RunTimeConcatStream (foldLength x) x
  (CompileTimeStreamType, CompileTimeStreamType) -> CompileTimeConcatStream x


type family CanNormalConcat (l :: Length) = (b :: Bool) where
  CanNormalConcat Infinite = True
  CanNormalConcat Unknown = True
  CanNormalConcat RunTime = True
  CanNormalConcat (CompileTime _) = False
  CanNormalConcat Empty = True

foldLength :: Stream (Known l1) (RunTimeStream a) -> Int
foldLength x = foldl' (+) 0 (fmap length x)

concat :: (CanNormalConcat l ~ True) => Stream l (Stream l a) -> Stream l a
concat x = case (getStreamType x) of
  InfiniteStreamType -> safeHead x
  UnknownStreamType -> UnknownConcatStream x
  RunTimeStreamType -> RunTimeConcatStream (foldLength x) x
  EmptyStreamType -> EmptyStream

concatMap :: (CanNormalConcat l ~ True) => (a -> (Stream l b)) -> Stream l a -> Stream l b
concatMap f = concat . (fmap f)

monadAp :: (Monad m) => m (a -> b) -> m a -> m b
monadAp fs xs = fs >>= (\f -> fmap f xs)

instance Applicative (Stream Unknown) where
  pure x = UnknownUntypedStream (SingletonStream x)
  (<*>) = monadAp

instance Monad (Stream Unknown) where
  (>>=) x f = concatMap f x

instance Applicative (Stream RunTime) where
  pure x = RunTimeUntypedStream (SingletonStream x)
  (<*>) = monadAp

instance Monad (Stream RunTime) where
  (>>=) x f = concatMap f x


instance Monoid (Stream Unknown a) where
  mempty = UnknownUntypedStream EmptyStream
  mappend = append
  mconcat x = concat (toStream x)

instance Monoid (Stream RunTime a) where
  mempty = RunTimeUntypedStream EmptyStream
  mappend = append

instance Monoid (Stream Empty a) where
  mempty = EmptyStream
  mappend _ _ = EmptyStream
  mconcat _ = EmptyStream

instance Semigroup (Stream Unknown a) where
  stimes n e = concat (UnknownUntypedStream (replicate n e))

instance Semigroup (Stream RunTime a) where
  stimes n e = concat (replicate n e)

instance Semigroup (Stream Empty a) where
  stimes _ _ = EmptyStream

instance Semigroup (Stream Infinite a) where
  (<>) = const
  stimes _ e = e

instance Alternative (Stream Unknown) where
  empty = UnknownUntypedStream EmptyStream
  (<|>) = append

instance Alternative (Stream RunTime) where
  empty = RunTimeUntypedStream EmptyStream
  (<|>) = append

instance (Alternative (Stream l), Monad (Stream l)) => MonadPlus (Stream l)

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 f g x y = f (g x y)

wrap :: Stream l a -> UnknownStream a
wrap x = case (getStreamType x) of
  InfiniteStreamType -> UnknownUntypedStream x
  UnknownStreamType -> x
  RunTimeStreamType -> case x of
    RunTimeUntypedStream x -> UnknownUntypedStream x
    _ -> UnknownUntypedStream x
  CompileTimeStreamType -> UnknownUntypedStream x
  EmptyStreamType -> UnknownUntypedStream EmptyStream

wrapRunTime :: Stream (Known l) a -> RunTimeStream a
wrapRunTime x = case (getStreamType x) of
  RunTimeStreamType -> x
  CompileTimeStreamType -> RunTimeUntypedStream x
  EmptyStreamType -> RunTimeUntypedStream EmptyStream

compileTimeLength :: forall n t. (KnownNat n) => Stream (CompileTime (n :: Nat)) t -> Int
compileTimeLength _ = fromInteger (natVal (Proxy :: Proxy n))


instance Foldable (Stream l) where
  foldl' f z = go where
    go (RunTimeSingleStream n sf s) = go' n z s where
      go' n acc s =
        case n of
          0 -> acc
          _ ->
            let
              (x, next_state) = sf s
              next_acc = f acc x
            in
              next_acc `seq` go' (n-1) next_acc next_state
    go xs = foldr f' id xs z
      where f' x k z' = k $! f z' x

  foldr :: forall a b l. (a -> b -> b) -> b -> Stream l a -> b
  foldr f z = go where
    go :: forall l. Stream l a -> b
    go (InfiniteSingleStream sf s) = h s where
      h s' = let (r, next_s) = sf s' in r `f` (h next_s)
    go (UnknownSingleStream sf s) = h s where
      h s' = case sf s' of
        Just (r, next_s) -> r `f` (h next_s)
        Nothing -> z
    go (RunTimeSingleStream n sf s) = foldrFixedLength f z sf n s
    go str@(CompileTimeSingleStream sf s) = foldrFixedLength f z sf (compileTimeLength str) s

    go (InfiniteAppendStream i1 i2) = foldrTwo i1 i2
    go (UnknownAppendStream i1 i2) = foldrTwo i1 i2
    go (RunTimeAppendStream i1 i2) = foldrTwo i1 i2
    go (CompileTimeAppendStream i1 i2) = foldrTwo i1 i2

    go EmptyStream = z
    go (SingletonStream e) = e `f` z
    go (UnknownUntypedStream x) = go x
    go (RunTimeUntypedStream x) = go x

    go (InfiniteConstantStream x) = forever (f x)
    go (UnknownConstantStream sf s x) = h s where
      h s' = case sf s' of
        Just next_s -> x `f` (h next_s)
        Nothing -> z
    go (RunTimeConstantStream n x) = applyNTimes n (f x)
    go s@(CompileTimeConstantStream x) = applyNTimes (length s) (f x)

    go (CompileTimeFoldableStream g l) = foldr (f . g) z l
    go (RunTimeFoldableStream _ g l) = foldr (f . g) z l
    go (UnknownFoldableStream g l) = foldrMaybe (\e s -> fmap (\x -> f x s) (g e)) l
    go (InfiniteFoldableStream g l) = foldr (f . g) z l

    go (CompileTimeZipStream g x y) = foldr f z (Prelude.zipWith g (toList x) (toList y))
    go (RunTimeZipStream g x y) = foldr f z (Prelude.zipWith g (toList x) (toList y))
    go (UnknownZipStream g x y) = (foldr f z . catMaybes) (Prelude.zipWith g (toList x) (toList y))
    go (InfiniteZipStream g x y) = foldr f z (Prelude.zipWith g (toList x) (toList y))

    go (CompileTimeConcatStream l) = concatFoldr l
    go (RunTimeConcatStream _ l) = concatFoldr l
    go (UnknownConcatStream l) = concatFoldr l
    go (InfiniteConcatStream l) = concatFoldr l

    go (CompileTimeLazyMemotisedStream x) = foldr f z x
    go (RunTimeLazyMemotisedStream x) = foldr f z x
    go (UnknownLazyMemotisedStream x) = foldr f z x
    go (InfiniteLazyMemotisedStream x) = foldr f z x

    go (CompileTimeStrictMemotisedStream x) = foldr f z (WrappedMonoFoldable x)
    go (RunTimeStrictMemotisedStream x) = foldr f z (WrappedMonoFoldable x)

    applyNTimes :: Int -> (b -> b) -> b
    applyNTimes n f = go z n where
      go acc 0 = acc
      go acc i = go (f acc) (i - 1)

    forever :: (b -> b) -> b
    forever f = go z where
      go acc = go (f acc)

    foldrTwo :: forall l1 l2. Stream l1 a -> Stream l2 a -> b
    foldrTwo i1 i2 = foldr f (foldr f z i2) i1

    foldrMaybe :: forall t a. (Foldable t) => (a -> b -> Maybe b) -> t a -> b
    foldrMaybe f = foldr g z where
      g e s = case f e s of
        Just next_s -> next_s
        Nothing -> s

    foldrFixedLength :: forall a b s. (a -> b -> b) -> b -> (s -> (a,s)) -> Int -> s -> b
    foldrFixedLength f z sf = go where
      go (0 :: Int) _ = z
      go n s = let (r, next_s) = sf s in r `f` (go (n-1) next_s)

    concatFoldr :: forall l1 l2. Stream l1 (Stream l2 a) -> b
    concatFoldr l = foldr (.) id (fmap (\l' z' -> foldr f z' l') l) z

  length x = case (safeLength x) of
    KnownSafeLength n -> n
    UnknownSafeLength n -> n
    InfiniteSafeLength -> error "Length is infinite."

  null x = case (getStreamType x) of
    InfiniteStreamType -> False
    UnknownStreamType -> foldr (\_ _ -> False) True x
    RunTimeStreamType -> case x of
      RunTimeAppendStream x y -> null x && null y
      RunTimeUntypedStream x -> null x
      RunTimeConcatStream _ x -> all null x
      RunTimeSinglePattern -> isLength0
      RunTimeFoldablePattern -> isLength0
      RunTimeConstantPattern -> isLength0
      RunTimeZipPattern -> isLength0
      RunTimeLazyMemotisedPattern -> isLength0
      RunTimeStrictMemotisedPattern -> isLength0
      _ -> patternSynonymCatchAll
    CompileTimeStreamType -> isLength0
    EmptyStreamType -> True
    where
      isLength0 = (length x) == 0



data SafeLength = KnownSafeLength Int | UnknownSafeLength Int | InfiniteSafeLength

addSafeLength :: SafeLength -> SafeLength -> SafeLength
addSafeLength InfiniteSafeLength _ = InfiniteSafeLength
addSafeLength _ InfiniteSafeLength = InfiniteSafeLength
addSafeLength (KnownSafeLength x) (KnownSafeLength y) = KnownSafeLength (x+y)
addSafeLength (KnownSafeLength x) (UnknownSafeLength y) = UnknownSafeLength (x+y)
addSafeLength (UnknownSafeLength x) (KnownSafeLength y) = UnknownSafeLength (x+y)
addSafeLength (UnknownSafeLength x) (UnknownSafeLength y) = UnknownSafeLength (x+y)

minSafeLength :: SafeLength -> SafeLength -> SafeLength
minSafeLength InfiniteSafeLength y = y
minSafeLength x InfiniteSafeLength = x
minSafeLength (KnownSafeLength x) (KnownSafeLength y) = KnownSafeLength (min x y)
minSafeLength (KnownSafeLength x) (UnknownSafeLength y) = UnknownSafeLength (min x y)
minSafeLength (UnknownSafeLength x) (KnownSafeLength y) = UnknownSafeLength (min x y)
minSafeLength (UnknownSafeLength x) (UnknownSafeLength y) = UnknownSafeLength (min x y)


knownLength :: forall l a. Stream (Known l) a -> Int
knownLength x = case (safeLength x) of
  KnownSafeLength n -> n
  _ -> error "knownLength should always be a KnownSafeLength"

safeLength :: forall l a. Stream l a -> SafeLength
safeLength x = case (getStreamType x) of
  InfiniteStreamType -> InfiniteSafeLength
  UnknownStreamType -> case x of
    UnknownAppendStream x y -> (safeLength x) `addSafeLength` (safeLength y)
    UnknownUntypedStream x -> safeLength x
    UnknownConcatStream x -> foldl' addSafeLength (KnownSafeLength 0) (fmap safeLength x)
    UnknownLazyMemotisedStream x -> UnknownSafeLength (length x)
    UnknownSinglePattern -> foldLength
    UnknownFoldablePattern -> foldLength
    UnknownZipPattern -> foldLength
    UnknownConstantPattern -> foldLength
    _ -> patternSynonymCatchAll
  RunTimeStreamType -> KnownSafeLength (lengthRunTime x)
  CompileTimeStreamType -> KnownSafeLength (compileTimeLength x)
  EmptyStreamType -> KnownSafeLength 0
  where
    foldLength = UnknownSafeLength (foldl' (\c _ -> c+1) 0 x)

lengthRunTime :: forall l a. Stream (Known l) a -> Int
lengthRunTime x = case getStreamType x of
  RunTimeStreamType -> case x of
    (RunTimeSingleStream n _ _) -> n
    (RunTimeAppendStream x y) -> lengthRunTime x + lengthRunTime y
    (RunTimeFoldableStream n _ _) -> n
    (RunTimeConstantStream n _) -> n
    (RunTimeUntypedStream x) -> lengthRunTime x
    (RunTimeZipStream _ x y) -> case (minSafeLength (safeLength x) (safeLength y)) of
      KnownSafeLength n -> n
      _ -> error "Length of a RunTimeZipStream should always be known"
    (RunTimeConcatStream n _) -> n
    (RunTimeLazyMemotisedStream x) -> length x
    (RunTimeStrictMemotisedStream x) -> Unboxed.length x
  CompileTimeStreamType-> compileTimeLength x
  EmptyStreamType -> 0


iterate :: (a -> a) -> a -> Stream Infinite a
iterate f x = InfiniteSingleStream g x where
  g x = let y = f x in (y,y)

repeat :: a -> Stream Infinite a
repeat = InfiniteConstantStream

cycle :: Stream l a -> Stream Infinite a
cycle x = case getStreamType x of
  InfiniteStreamType -> x
  _ -> InfiniteConcatStream (InfiniteConstantStream x)

{-# INLINE myfoldl' #-}
myfoldl' :: (Int -> Int -> Int) -> Int -> Stream RunTime Int -> Int
myfoldl' f z x = foldl' f z x

{-# INLINE myfmap #-}
myfmap :: (Int -> Int) -> Stream RunTime Int -> Stream RunTime Int
myfmap f x = fmap f x

main :: IO ()
--main = print fast --(sumSquare xs)
main = print ans --sumL xs
--main = print $ sumL xs

ans :: Int
ans = sumL xs

n :: Int
n = 100000000

f :: Int -> Int
f x = x*(x .&. 3)

xs :: RunTimeStream Int
xs = RunTimeSingleStream n (\x -> (x, x+1)) 1

sumL :: RunTimeStream Int -> Int
sumL x = myfoldl' (+) 0 (myfmap f x)

fast :: Int
fast = go 0 1 where
  go :: Int -> Int -> Int
  go acc i = if i > n then acc else let next_acc = acc + f i in next_acc `seq` go next_acc (i + 1)
