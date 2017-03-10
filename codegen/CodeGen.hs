module Data.Stream.CodeGen where

import Control.Applicative (liftA2)
import Data.Semigroup (stimes)

-- First tuple is var when being placed into and append constructor, second tuple is when being read itself
-- i.e. (\x -> (ProxyType x), \x -> x@(ProxyType x))
type ConstructorType = ((String -> String), (String -> String))
type AppendConstructorType = (String -> String -> String)

genCode :: String

genCode =
  genCodeAll
  where
    appendFuncSig = appendFuncName ++ " :: Stream l1 a -> Stream l2 a -> Stream (AppendLength l1 l2) a\n"
    wrap x = appendFuncName ++ " " ++ x ++ "\n"
    wrapProxyFunc x = proxyFuncName ++ " " ++ x ++ "\n"
    genCodeNormal :: AppendConstructorType -> [ConstructorType] -> [ConstructorType] -> String
    genCodeNormal appcon l1 l2 = concat (liftA2 genCode (map fst l1) (map fst l2)) where
      genCode con1 con2 = wrap $ con1 firstVar ++ " " ++ con2 secondVar ++ " = " ++ appcon firstVar secondVar

    genCodeInfiniteFirst :: String
    genCodeInfiniteFirst = concatMap go (map snd infiniteStreams) where
      go con = wrap $ con firstVar ++ " _ = " ++ firstVar

    genCodeNullFirst :: [ConstructorType] -> String
    genCodeNullFirst l = concat (map genCode (map snd l)) where
      genCode con = wrap $ nullConstructor ++ " " ++ con secondVar ++ " = " ++ secondVar

    genCodeNullSecond :: [ConstructorType] -> String
    genCodeNullSecond l = concat (map genCode (map snd l)) where
      genCode con = wrap $ con firstVar ++ " " ++ nullConstructor ++ " = " ++ firstVar

    genCodeNullBoth :: String
    genCodeNullBoth = wrap $  nullConstructor ++ " " ++ nullConstructor ++ " = " ++ nullConstructor

    genCodeNull l = genCodeNullFirst l ++ genCodeNullSecond l

    genAllNull = genCodeNullBoth ++ genCodeNullFirst infiniteStreams ++ genCodeNull (unknownStreams ++ runTimeStreams ++ compileTimeStreams)

    proxySig = proxyFuncName ++ " :: Stream l a -> ProxyStream l a\n"
    proxyLine con stream = wrapProxyFunc $ stream firstVar ++ " = " ++ con firstVar
    proxyLines con streams = concatMap (proxyLine con) (map snd streams)
    proxyNull = wrapProxyFunc $ nullConstructor ++ " = " ++ compileTimeProxy nullConstructor

    genCodeAll =
      appendFuncSig ++
      genCodeInfiniteFirst ++
      genAllNull ++
      genCodeNormal infiniteAppend unknownStreams infiniteStreams ++
      genCodeNormal infiniteAppend runTimeStreams infiniteStreams ++
      genCodeNormal infiniteAppend compileTimeStreams infiniteStreams ++
      genCodeNormal unknownAppend unknownStreams unknownStreams ++
      genCodeNormal unknownAppend unknownStreams runTimeStreams ++
      genCodeNormal unknownAppend unknownStreams compileTimeStreams ++
      genCodeNormal unknownAppend runTimeStreams unknownStreams ++
      genCodeNormal unknownAppend compileTimeStreams unknownStreams ++
      genCodeNormal runTimeAppend runTimeStreams runTimeStreams ++
      genCodeNormal runTimeAppend runTimeStreams compileTimeStreams ++
      genCodeNormal runTimeAppend compileTimeStreams runTimeStreams ++
      genCodeNormal compileTimeAppend compileTimeStreams compileTimeStreams ++
      "\n" ++
      proxySig ++
      proxyLines infiniteProxy infiniteStreams ++
      proxyLines unknownProxy unknownStreams ++
      proxyLines runTimeProxy runTimeStreams ++
      proxyLines compileTimeProxy compileTimeStreams ++
      proxyNull

dupPair x = (x,x)

c2 s x y = s ++ " " ++ x ++ " " ++ y
c1 v s = dupPair (c1' v s)
c1' v s x = x ++ "@(" ++ s ++ stimes v " _" ++ ")"
c1noat s = (\x -> "(" ++ s ++ " " ++ x ++ ")", c1' 1 s)
cProxy s x = s ++ " " ++ x

appendFuncName = "appendFlat"
proxyFuncName = "toProxyStream"
firstVar = "x"
secondVar = "y"
nullConstructor = "NullStream"
infiniteAppend = c2 "InfiniteAppendStream"
unknownAppend = c2 "UnknownAppendStream"
runTimeAppend = c2 "RunTimeAppendStream"
compileTimeAppend = c2 "CompileTimeAppendStream"
infiniteProxy = cProxy "InfiniteStream"
unknownProxy = cProxy "UnknownStream"
runTimeProxy = cProxy "RunTimeStream"
compileTimeProxy = cProxy "CompileTimeStream"
nullStreams = ["NullStream"]
infiniteStreams = [c1 2 "InfiniteSingleStream", c1 2 "InfiniteAppendStream", c1 2 "InfiniteFoldableStream", c1 1 "InfiniteConstantStream"]
unknownStreams = [c1 2 "UnknownSingleStream", c1 2 "UnknownAppendStream", c1 2 "UnknownFoldableStream", c1noat "UnknownUntypedStream"]
runTimeStreams = [c1 3 "RunTimeSingleStream", c1 2 "RunTimeAppendStream", c1 3 "RunTimeFoldableStream", c1 2 "RunTimeConstantStream", c1noat "RunTimeUntypedStream"]
compileTimeStreams = [c1 2 "CompileTimeSingleStream", c1 2 "CompileTimeAppendStream", c1 2 "CompileTimeFoldableStream", c1 1 "CompileTimeConstantStream", c1 1 "SingletonStream"]
