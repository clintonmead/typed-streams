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

{-|
The motivation of this library is at least partly demostrated by the following problem with lists:

Consider the following code (which is taken from Tests.hs from this package btw):

> f :: Int -> Int
> f x = x*(x .&. 3)
>
> g :: Int -> Int
> g x = x*(x .&. 7)

@f@ and @g@ are just silly example functions, which are effectively:

> f x = x * (x mod 8)
> g x = x * (x mod 16)

Now lets say we want to take some \"list\", apply f to it, apply g to it,
append both these together, and fold them. A straightforward way would be this:

> sumG :: (Functor t, Foldable t, Semigroup (t Int)) => t Int -> Int
> sumG x = foldl' (+) 0 ((fmap f x) <> (fmap g x))

For comparison sake, lets write a hand written version of this function:

> fast :: Int -> Int
> fast n = go g (go f 0 1 n) 1 n where
>   go :: (Int -> Int) -> Int -> Int -> Int -> Int
>   go f = go' where
>     go' :: Int -> Int -> Int -> Int
>     go' acc s i = if i == 0 then acc else let next_acc = acc + f s in next_acc `seq` go' next_acc (s + 1) (i - 1)

What you will probably find is, at least with GHC 8.0.2 which I've tested it with:

> sumG [1..n]

is about ten times slower than

> fast n

Even though they should be doing the same thing.

But, using this stream library, and 'Data.Generic.Enum.EnumFromTo' from another package, you can write:

> sumG (enumFromTo 1 n)

And this runs almost as fast as the handwritten code.

Now you may be able to get this speed out of ordinary lists with some fancy rewrite rules
(and indeed this Stream library does have a few fancy rewrite rules itself) there more
theortical advantages that Data.Stream.Stream' could have over lists.

Unlike ordinary lists, streams do not store the data directly. They just store a way to generate the data.

What does this mean?

At the moment, the main way to process a stream is to fold over it. You can't really deconstruct it
step by step. But generally folds give you enough power to process a list.

Also, if you fold over a stream twice, you'll have to recalculate it. This is a good and bad thing,
It can be bad because you have to recalculate, but it's good because you won't use up memory.
For many lists used in practice, they're simple enough to regenerate instead of storing, and it prevents
huge heap usage from code like this:

> average x = (foldl' (+) 0 x) / (length x)

There's other advantages to this approach. Firstly, appending streams is always a constant time operation.
Always. Even if the first stream is infinite. All appending streams does is generate a new "stream" which has
the two appended streams as data items.

Actually, our stream data type is more sophisticated than this. A 'Stream' is a type of two variables, the second
is the element type as usual, but the first is the \"Length\". Streams can be the following lengths:

* Infinite
* Unknown
* RunTime
* CompileTime
* Empty

Infinite streams are well, infinte, not much to say here.

Unknown streams are streams we don't know the length of. They could be infinite or finite. Ordinary lists are like this.

RunTime streams have a defined finite length, which takes constant time to access.

CompileTime streams have their length as a compile time factor.

Empty streams are well, empty.

Having these different types can be useful. We might want a safe \"toVector\" function that takes only RunTime streams,
and immediately allocates the vector to that size before filling it.

But 'Stream' is indeed a GADT.

Currently there are 34 different types of streams. These range from simple streams just with a state
and a \"next_state\" function, to streams representing appended streams, concatenated streams, etc.

There's even streams that are a wrapper for 'Foldable' types, so instead of converting everything to a list,
you can just wrap your data in a stream and combine data of all different types seemlessly.

I believe there's lots of opportunity to optimise this library. Potentially
(if I got to understand the GHC API better) streams could carry around code blocks, which could compile
just in time (JIT) when required. This could allow for fast code to be generated in situations where there
are complex transformations, perhaps based on runtime branching, which the inliner can miss.

However, currently optimisation is limited. Indeed, the only optimisation I've to optimise the example
given in this documentation. But it does show the potential, and it is an extensible framework.
-}

module Data.Stream.Typed (
  Stream, CompileTimeStream, RunTimeStream, UnknownStream, InfiniteStream,
  CompileTime, RunTime, Length(Unknown, Infinite),
  ToStream(toStream), Element,
  -- $foldableToStreamDocs
  runTimeFoldableToStream,
  runTimeFoldableToStreamWithLength,
  unknownFoldableToStream,
  empty,
  singleton,
  AppendLength, append,
  -- $zipDocs
  zip, zipWith, ZipLength,
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
  lengthRunTime,
  safeHead, unsafeHead, maybeHead,
  mixedConcat, ConcatLength,
  memotise, strictMemotise,
  wrapUnknown,
  wrapRunTime
  ) where

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
  Integral, fromIntegral,
  flip
  )

import Control.Applicative (
  Applicative, pure, (<*>),
  Alternative, (<|>)
  )

import qualified Control.Applicative

import Control.Monad (
  Monad, (>>=), return,
  MonadPlus, mplus
  )

import Data.Monoid (
  Monoid, mempty, mappend, mconcat
  )

import Control.Monad.Fix (fix)


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

import qualified GHC.Exts

import Data.Type.Bool (type If)

import Control.Monad (foldM_)

import Data.MonoTraversable.WrapMonoFoldable (WrappedMonoFoldable(WrappedMonoFoldable))

import Data.MonoTraversable (Element)

import Data.Semigroup (
  Semigroup, (<>), stimes
  )

import Data.Array (Array, )

import GHC.Exts (Item, lazy)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS

import Data.Word (Word8)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V hiding (length)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VU hiding (length)

import Data.Vector.Unboxed (Unbox)

import Data.Generic.Enum (EnumFromTo(enumFromStepCount), EnumFrom(enumFromStep), Enum(type EnumNumT, type EnumIntegralT), fromEnum, toEnum)
import qualified Data.Generic.Enum as GE

data Length = Unknown | Infinite | Known KnownType
data KnownType = RunTimeLength | CompileTimeLength CompileTimeLengthType

data CompileTimeLengthType = NatLength Nat | Zero

type RunTime = Known RunTimeLength
type CompileTime n = Known (CompileTimeLength (NatLength n))
type Empty = Known (CompileTimeLength Zero)

type InfiniteStream a = Stream Infinite a
type UnknownStream a = Stream Unknown a
type RunTimeStream a = Stream RunTime a
type CompileTimeStream n a = Stream (CompileTime n) a
type EmptyStream a = Stream Empty a

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

  CompileTimeLazyMemotisedStream :: (KnownNat n) => V.Vector a -> CompileTimeStream n a
  RunTimeLazyMemotisedStream :: V.Vector a -> RunTimeStream a
  UnknownLazyMemotisedStream :: [a] -> UnknownStream a
  InfiniteLazyMemotisedStream :: [a] -> InfiniteStream a

  CompileTimeStrictMemotisedStream :: (KnownNat n, Unbox a) => VU.Vector a -> CompileTimeStream n a
  RunTimeStrictMemotisedStream :: (Unbox a) => VU.Vector a -> RunTimeStream a

--  FiniteEnumStream :: (Enum a) => a -> EnumNumT a -> EnumIntegral a -> RunTimeStream a
--  InfiniteEnumStream :: (Enum a) => a -> EnumNumT a -> RunTimeStream a

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

data StreamType (x :: Length) where
  InfiniteStreamType :: StreamType Infinite
  UnknownStreamType :: StreamType Unknown
  RunTimeStreamType :: StreamType RunTime
  CompileTimeStreamType :: (KnownNat n) => StreamType (CompileTime n)
  EmptyStreamType :: StreamType Empty

empty :: EmptyStream a
empty = EmptyStream

singleton :: a -> CompileTimeStream 1 a
singleton = SingletonStream

replicate :: (Integral b) => b -> a -> RunTimeStream a
replicate n = RunTimeConstantStream (fromIntegral n)

unfoldr :: (b -> Maybe (a, b)) -> b -> UnknownStream a
unfoldr = UnknownSingleStream

{- $foldableToStreamDocs
Both 'runTimeFoldableToStream' and 'unknownFoldableToStream' wraps a Foldable data into a stream.
Which one you use is a matter of choice, but generally you should use 'runTimeFoldableToStream'
for structures like Vector which have a fixed and constant time list operation, and
'unknownFoldableToStream' for structures like list, particularly when you don't yet know their length.

By default 'runTimeFoldableToStream' just calls 'length' to work out it's length, but if say, you've got
a list but you already know it's length (and that it's finite), then 'runTimeFoldableToStreamWithLength'
might be the more appropriate choice.
-}


runTimeFoldableToStream :: (Foldable t) => t a -> RunTimeStream a
runTimeFoldableToStream x = RunTimeFoldableStream (length x) id x

runTimeFoldableToStreamWithLength :: (Foldable t) => Int -> t a -> RunTimeStream a
runTimeFoldableToStreamWithLength n x = RunTimeFoldableStream n id x

unknownFoldableToStream :: (Foldable t) => t a -> UnknownStream a
unknownFoldableToStream = UnknownFoldableStream pure

type family LengthT a = (r :: Length)

{-|
Add instances to the 'toStream' class to allow for easy conversion to streams.
Technically you could just use 'runTimeFoldableToStream' and ,'unknownFoldableToStream' to wrap data in
streams, but with this approach you can specialise for particular datatypes if appropriate.
-}
class ToStream a where
  toStream :: a -> Stream (LengthT a) (Element a)

type instance LengthT [a] = Unknown
instance ToStream [a] where
  toStream x = UnknownFoldableStream pure x

type instance Element (Array i e) = e
type instance LengthT (Array i e) = RunTime
instance ToStream (Array i e) where
  toStream x = RunTimeFoldableStream (length x) id x

type instance LengthT BS.ByteString = RunTime
instance ToStream BS.ByteString where
  toStream x = RunTimeFoldableStream (BS.length x) id (WrappedMonoFoldable x)

type instance LengthT BSL.ByteString = RunTime
instance ToStream BSL.ByteString where
  toStream x = RunTimeFoldableStream ((fromIntegral . BSL.length) x) id (WrappedMonoFoldable x)

type instance LengthT (V.Vector a) = RunTime
instance ToStream (V.Vector a) where
  toStream x = RunTimeLazyMemotisedStream x

type instance LengthT (VU.Vector a) = RunTime
instance (Unbox a) => ToStream (VU.Vector a) where
  toStream x = RunTimeStrictMemotisedStream x

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

{-# INLINE [1] (<>-) #-}
(<>-) :: Stream l a -> Stream l a -> Stream l a
(<>-) x y = case (getStreamType x) of
  InfiniteStreamType -> x
  UnknownStreamType -> append x y
  RunTimeStreamType -> append x y
  CompileTimeStreamType -> error "This should never happen as this function should only be called by an appropriate rewrite rule."
  EmptyStreamType -> EmptyStream


{-|
Whilst appending two streams of the same type always results in the same type,
appending two streams of different types can always be done, with the result type selected as appropriately as
possible.
-}
{-# INLINE append #-}
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

{- $zipDocs
Both 'zip' and 'zipWith' aren't optimised currently, they just convert both sides to lists and zip them sadly.
-}

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

foldableToVector :: Foldable t => t a -> V.Vector a
foldableToVector l =
  V.create
  (
    do
      v <- V.new (length l)
      let f i x = V.write v i x >> return (i+1)
      foldM_ f 0 l
      return v
  )

foldableToUnboxedVector :: (Unbox a) => Foldable t => t a -> VU.Vector a
foldableToUnboxedVector l =
  VU.create
  (
    do
      v <- VU.new (length l)
      let f i x = VU.write v i x >> return (i+1)
      foldM_ f 0 l
      return v
  )

{-|
As discussed in the intro to this module, by default streams when evaluated don't store their data.
'memotise' is effectively an \"id\" style function, but it takes the stream and stores it in either a
Vector or list. For @RunTimeStreams@, we use a Vector, as we know the length, but for @UnknownStreams@ and
@InfiniteStreams@ we use a list.
-}
memotise :: Stream l a -> Stream l a
memotise x = case getStreamType x of
  InfiniteStreamType -> case x of
    InfiniteLazyMemotisedStream _ -> x
    _ -> InfiniteLazyMemotisedStream (toList x)
  UnknownStreamType -> case x of
    UnknownLazyMemotisedStream _ -> x
    UnknownUntypedStream x -> wrapUnknown (memotise x)
    _ -> UnknownLazyMemotisedStream (toList x)
  RunTimeStreamType -> case x of
    RunTimeLazyMemotisedStream _ -> x
    RunTimeStrictMemotisedStream _ -> x
    RunTimeUntypedStream x -> wrapRunTime (memotise x)
    _ -> RunTimeLazyMemotisedStream (foldableToVector x)
  CompileTimeStreamType -> CompileTimeLazyMemotisedStream (foldableToVector x)
  EmptyStreamType -> EmptyStream

{-|
'strictMemotise' can be used for streams of Unboxed types. It then stores the data in
an unboxed vector. Note that this only works for streams of RunTime or CompileTime length,
obviously we can't put an infinite length vector in a vector, and we're not sure if unknown length
vectors are finite.

So in the case of infinite or unknown vectors, we just fall back to the normal 'memotise' behaviour.
-}
strictMemotise :: Unbox a => Stream l a -> Stream l a
strictMemotise x = case getStreamType x of
  InfiniteStreamType -> memotise x
  UnknownStreamType -> memotise x
  RunTimeStreamType -> case x of
    RunTimeStrictMemotisedStream _ -> x
    RunTimeUntypedStream x -> wrapRunTime (strictMemotise x)
    _ -> RunTimeStrictMemotisedStream (foldableToUnboxedVector x)
  CompileTimeStreamType -> CompileTimeStrictMemotisedStream (foldableToUnboxedVector x)
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
  go (InfiniteSingleStream sf s) = wrapUnknown (InfiniteSingleStream h s) where
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

  go (UnknownUntypedStream x) = wrapUnknown (go x)
  go (RunTimeUntypedStream x) = wrapUnknown (go x)

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


  filterConstant e = if f e then wrapUnknown x else emptyStream
  emptyStream = UnknownUntypedStream EmptyStream

{-# INLINE [1] fmap' #-}
fmap' :: forall a b l. (a -> b) -> Stream l a -> Stream l b
fmap' f = go where
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

  go (CompileTimeConcatStream l) = CompileTimeConcatStream (fmap' go l)
  go (RunTimeConcatStream n l) = RunTimeConcatStream n (fmap' go l)
  go (UnknownConcatStream l) = UnknownConcatStream (fmap' go l)
  go (InfiniteConcatStream l) = InfiniteConcatStream (fmap' go l)


  go (CompileTimeLazyMemotisedStream x) = CompileTimeFoldableStream f x
  go (RunTimeLazyMemotisedStream x) = RunTimeFoldableStream (length x) f x
  go (UnknownLazyMemotisedStream x) = UnknownFoldableStream (pure . f) x
  go (InfiniteLazyMemotisedStream x) = InfiniteFoldableStream f x

  go (CompileTimeStrictMemotisedStream x) = CompileTimeFoldableStream f (WrappedMonoFoldable x)
  go (RunTimeStrictMemotisedStream x) = RunTimeFoldableStream (VU.length x) f (WrappedMonoFoldable x)


instance Functor (Stream l) where
  fmap :: forall a b l. (a -> b) -> Stream l a -> Stream l b
  fmap = fmap'

  (<$) :: forall l a b. a -> Stream l b -> Stream l a
  (<$) e = go where
    go :: forall l. Stream l b -> Stream l a
    go x = case (getStreamType x) of
      InfiniteStreamType -> InfiniteConstantStream e
      UnknownStreamType -> case x of
        (UnknownAppendStream x y) -> UnknownAppendStream (go x) (go y)
        (UnknownUntypedStream x) -> wrapUnknown (go x)
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

{-|
Just like Prelude's 'Prelude.head', errors out if there's a problem.
-}
unsafeHead :: Foldable t => t a -> a
unsafeHead = foldr const (error "Empty Foldable")

{-|
'safeHead' will only work on types which are guarenteed to have a head, like infinite streams
and compile time streams of length at least 1.
-}
safeHead :: (SafeHead l ~ True) => Stream l a -> a
safeHead = unsafeHead

{-|
Returns @Just a@ if list has a head, @Nothing@ otherwise.
-}
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

{-|
'mixedConcat' is like the usual \"concat\", i.e. @[[a]] -> [a]@ except it works with nested
streams of different types, e.g. @RunTimeStream (UnknownStream a)@
-}
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

{-|
'concat' like a restricted version of 'mixedConcat' where the input and output types are the same.

Note 'concat' does not work on streams with compile time length, as with these streams the length is
included in the type so obviously concatenating them changes the type.
-}
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

{-|
Changes the type of any streams length to 'UnknownStream'.

Note that whilst now you can not distinguish this stream's length using the type system, it still
retains all it's previous behaviour. So if you 'wrapUnknown' a run time length stream, it's length function
will still work in constant time.
-}
wrapUnknown :: Stream l a -> UnknownStream a
wrapUnknown x = case (getStreamType x) of
  InfiniteStreamType -> UnknownUntypedStream x
  UnknownStreamType -> x
  RunTimeStreamType -> case x of
    RunTimeUntypedStream x -> UnknownUntypedStream x
    _ -> UnknownUntypedStream x
  CompileTimeStreamType -> UnknownUntypedStream x
  EmptyStreamType -> UnknownUntypedStream EmptyStream

{-|
Like 'wrapUnknown' but instead to 'RunTimeStream'.

Of course, only runtime, compile time or empty streams can be converted to runtime streams, because runtime streams
must know their length.
-}
wrapRunTime :: Stream (Known l) a -> RunTimeStream a
wrapRunTime x = case (getStreamType x) of
  RunTimeStreamType -> x
  CompileTimeStreamType -> RunTimeUntypedStream x
  EmptyStreamType -> RunTimeUntypedStream EmptyStream

compileTimeLength :: forall n t. (KnownNat n) => Stream (CompileTime (n :: Nat)) t -> Int
compileTimeLength _ = fromInteger (natVal (Proxy :: Proxy n))

data FoldInlineStage = FirstCall | ProxyCall | RecursiveCall

type family FoldInlineProxyNextStage x = (r :: FoldInlineStage) where
  FoldInlineProxyNextStage FirstCall = ProxyCall
  FoldInlineProxyNextStage _ = RecursiveCall

{-
There's some fancy optimisation going on here. What I noticed is that GHC can be amazingly fast if it can
inline. But it can't inline recursive functions. But unfortunately any branch of the function being
recursive makes it ineligable for inlining. I'd like to inline the simple cases.

So how this works is that all the functions get a dummy argument. What you'll notice is that
these functions are never called recursively with the dummy argument 'FirstCall'.

So if we specialise with this dummy argument, it will be non-recursive and inline.

Inlining is very important because it allows for all sorts of further optimisations.

Note I've only optimised foldl' like this. There is more work to be done!
-}
{-# INLINE [1] foldl''' #-}
foldl''' :: forall a b l. (b -> a -> b) -> b -> Stream l a -> b
foldl''' = goF' (Proxy :: Proxy FirstCall) where
  {-# SPECIALISE INLINE goF' :: Proxy FirstCall -> (b -> a -> b) -> b -> Stream l a -> b #-}
  goF' :: forall a l callStage. Proxy (callStage :: FoldInlineStage) -> (b -> a -> b) -> b -> Stream l a -> b
  goF' cs f = goZ' cs where
    {-# SPECIALISE INLINE goZ' :: Proxy FirstCall -> b -> Stream l a -> b #-}
    goZ' :: forall l callStage. Proxy (callStage :: FoldInlineStage) -> b -> Stream l a -> b
    goZ' cs z = go' cs where
      {-# SPECIALISE INLINE go' :: Proxy FirstCall -> Stream l a -> b #-}
      {-# SPECIALISE INLINE go' :: Proxy ProxyCall -> Stream l a -> b #-}
      go' :: forall l callStage. Proxy (callStage :: FoldInlineStage) -> Stream l a -> b
      go' _ x = case getStreamType x of
        EmptyStreamType -> case x of
          EmptyStream -> z
        CompileTimeStreamType -> case x of
          SingletonStream e -> z `f` e
          CompileTimeConstantStream e -> applyNTimesL e (compileTimeLength x)
          CompileTimeSingleStream sf s -> foldl'FixedLength sf (compileTimeLength x) s
          CompileTimeAppendStream x y -> foldl'Two x y
          CompileTimeFoldableStream g l -> doFoldableL g l
          CompileTimeZipStream g x y -> foldl' f z (Prelude.zipWith g (toList x) (toList y))
          CompileTimeConcatStream x -> concatFoldl' x
          CompileTimeLazyMemotisedStream x -> foldl' f z x
          CompileTimeStrictMemotisedStream x -> foldl' f z (WrappedMonoFoldable x)
        RunTimeStreamType -> case x of
          RunTimeSingleStream n sf s -> foldl'FixedLength sf n s
          RunTimeAppendStream x y -> foldl'Two x y
          RunTimeUntypedStream x -> goProxy x
          RunTimeConstantStream n e -> applyNTimesL e n
          RunTimeFoldableStream _ g l -> doFoldableL g l
          RunTimeZipStream g x y -> foldl' f z (Prelude.zipWith g (toList x) (toList y))
          RunTimeConcatStream _ x -> concatFoldl' x
          RunTimeLazyMemotisedStream x -> foldl' f z x
          RunTimeStrictMemotisedStream x -> foldl' f z (WrappedMonoFoldable x)
        UnknownStreamType -> case x of
          UnknownSingleStream sf s -> h z s where
            h acc s = case sf s of
              Just (r, next_s) ->
                let next_acc = acc `f` r in next_acc `seq` h next_acc next_s
              Nothing -> acc
          UnknownAppendStream x y -> foldl'Two x y
          UnknownConstantStream sf s e -> applyWhileJustState (`f` e) sf z s
          UnknownFoldableStream g l -> foldl' h z l where
            h x y = case g y of
              Just y' -> f x y'
              Nothing -> x
          UnknownUntypedStream x -> goProxy x
          UnknownZipStream g x y -> (foldl' f z . catMaybes) (Prelude.zipWith g (toList x) (toList y))
          UnknownConcatStream x -> concatFoldl' x
          UnknownLazyMemotisedStream x -> foldl' f z x
        InfiniteStreamType -> error "Can't foldl' an infinite stream"


      go :: forall l. Stream l a -> b
      go = go' (Proxy :: Proxy RecursiveCall)
      goProxy :: forall l. Stream l a -> b
      goProxy = go' (Proxy :: Proxy (FoldInlineProxyNextStage callStage))
      goZ :: forall l. b -> Stream l a -> b
      goZ = goZ' (Proxy :: Proxy RecursiveCall)
      goF :: forall a l. (b -> a -> b) -> b -> Stream l a -> b
      goF = goF' (Proxy :: Proxy RecursiveCall)

      foldl'Two :: forall l1 l2. Stream l1 a -> Stream l2 a -> b
      foldl'Two x y = goZ (go x) y

      foldl'FixedLength :: forall s. (s -> (a,s)) -> Int -> s -> b
      foldl'FixedLength sf = go z where
        go acc n s = case n of
          0 -> acc
          _ ->
              let
                (x, next_state) = sf s
                next_acc = f acc x
              in
                next_acc `seq` go next_acc (n-1) next_state

      applyNTimesL :: a -> Int -> b
      applyNTimesL e n = applyNTimes (`f` e) z n

      doFoldableL :: Foldable t => (s -> a) -> t s -> b
      doFoldableL g l = foldl' (\x y -> f x (g y)) z l

      concatFoldl' :: forall l1 l2. Stream l1 (Stream l2 a) -> b
      concatFoldl' = goF goZ z

applyNTimes :: (a -> a) -> a -> Int -> a
applyNTimes f = go where
  go acc i = case i of
    0 -> acc
    _ -> let next_acc = f acc in next_acc `seq` go next_acc (i - 1)

applyWhileJustState :: (a -> a) -> (s -> Maybe s) -> a -> s -> a
applyWhileJustState f sf = go where
  go acc s = case sf s of
    Nothing -> acc
    Just next_state -> let next_acc = f acc in next_acc `seq` next_state `seq` go next_acc next_state

instance Foldable (Stream l) where
  foldl' = foldl'''

  foldr :: forall a b l. (a -> b -> b) -> b -> Stream l a -> b
  foldr f z = go where
    go :: forall l. Stream l a -> b
    go str@(CompileTimeSingleStream sf s) = foldrFixedLength sf (compileTimeLength str) s
    go (RunTimeSingleStream n sf s) = foldrFixedLength sf n s
    go (UnknownSingleStream sf s) = h s where
      h s' = case sf s' of
        Just (r, next_s) -> r `f` (h next_s)
        Nothing -> z
    go (InfiniteSingleStream sf s) = h s where
      h s' = let (r, next_s) = sf s' in r `f` (h next_s)

    go (InfiniteAppendStream i1 i2) = foldrTwo i1 i2
    go (UnknownAppendStream i1 i2) = foldrTwo i1 i2
    go (RunTimeAppendStream i1 i2) = foldrTwo i1 i2
    go (CompileTimeAppendStream i1 i2) = foldrTwo i1 i2

    go EmptyStream = z
    go (SingletonStream e) = e `f` z
    go (UnknownUntypedStream x) = go x
    go (RunTimeUntypedStream x) = go x

    go (InfiniteConstantStream e) = e `f` (error "foldr of infinite constant stream using function strict in it's second argument, this can only diverge")
    go (UnknownConstantStream sf s e) =
      let
        g = (e `f`)
      in
        case sf s of
          Nothing -> z
          Just next_s -> g (applyWhileJustState g sf z next_s)
    go (RunTimeConstantStream n e) = applyNTimesR e n
    go s@(CompileTimeConstantStream e) = applyNTimesR e (compileTimeLength s)

    go (CompileTimeFoldableStream g l) = foldr (f . g) z l
    go (RunTimeFoldableStream _ g l) = foldr (f . g) z l

    go (UnknownFoldableStream g l) = foldr h z l where
      h x y = case g x of
        Just x' -> f x' y
        Nothing -> y
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

    {-
      This function does a foldR on a constant list.

      Remember what foldr looks like on a constant list,
      here's an example with length 4, and `f` is our function.

      e `f` (e `f` (e `f` (e `f` z)))

      foldr is not strict, and can short circuit by being lazy in its second argument.

      However, `f` is pure, so it's laziness in the second argument depends entirely on the first.
      So once we know `f` is not lazy with first argument `e`, it's never going to be lazy.

      So we can strictly evaluate the rest at this point, without comprimising laziness.
      Which is hopefully nice for performance, or at least space usage.
      -}

    applyNTimesR :: a -> Int -> b
    applyNTimesR e n = case n of
      0 -> z
      _ ->
        let
          g = (e `f`)
        in
          g (applyNTimes g z (n-1))

    foldrTwo :: forall l1 l2. Stream l1 a -> Stream l2 a -> b
    foldrTwo i1 i2 = foldr f (foldr f z i2) i1

    foldrFixedLength :: forall s. (s -> (a,s)) -> Int -> s -> b
    foldrFixedLength sf = go where
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
    (RunTimeLazyMemotisedStream x) -> V.length x
    (RunTimeStrictMemotisedStream x) -> VU.length x
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

enumFromStepCount' :: (Enum a) => a -> EnumNumT a -> EnumIntegralT a -> RunTimeStream a
enumFromStepCount' start stepsize count = RunTimeSingleStream (fromIntegral count) (\x -> (x, toEnum (fromEnum x + stepsize))) start

enumFromStep' :: (Enum a) => a -> EnumNumT a -> InfiniteStream a
enumFromStep' start stepsize = InfiniteSingleStream (\x -> (x, toEnum (fromEnum x + stepsize))) start

type instance GE.Element (Stream l a) = a

instance (Enum a) => EnumFromTo (RunTimeStream a) where--
  enumFromStepCount = enumFromStepCount'

instance (Enum a) => EnumFromTo (UnknownStream a) where
  enumFromStepCount start stepsize count = wrapUnknown (enumFromStepCount' start stepsize count)

instance (Enum a) => EnumFrom (InfiniteStream a) where--
  enumFromStep = enumFromStep'

instance (Enum a) => EnumFrom (UnknownStream a) where
  enumFromStep start stepsize = wrapUnknown (enumFromStep' start stepsize)

{-# RULES
"protect fmap" fmap = fmap'
"protect foldl'" foldl' = foldl'''
"protect <>" (<>) = (<>-)

"fmap/semigroup" forall f xs ys. fmap' f (xs <>- ys) = (fmap' f xs) <>- (fmap' f ys)
"foldl'/semigroup" forall f z xs ys. foldl''' f z (xs <>- ys) = foldl''' f (foldl''' f z xs) ys
"foldl'/fmap" forall f z g x. foldl''' f z (fmap' g x) = let h x y = f x (g y) in foldl''' h z x
#-}

