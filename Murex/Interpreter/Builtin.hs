module Murex.Interpreter.Builtin where

import Import
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Murex.Data
import Murex.Interpreter.Values
import Murex.Syntax.Typeless

runBuiltin :: Builtin -> [Value] -> IO Value
runBuiltin PutChr [Data (MurexChar c)] = putChar c >> return (Data MurexUnit)
runBuiltin GetChr [] = Data . MurexChar <$> getChar
runBuiltin PutStr [Data (MurexSeq str)] = do
    putStr $ map fromMurexChar (toList str)
    return $ Data MurexUnit
runBuiltin GetStr [] = Data . MurexSeq . S.fromList . map toMurexChar <$> getLine


startEnv :: AList Symbol AST
startEnv = [ (intern "putChr", Lambda [varX] $ Apply [Builtin PutChr, Var varX])
           , (intern "getChr", Lambda [varX] $ Apply [Builtin GetChr])
           , (intern "putStr", Lambda [varX] $ Apply [Builtin PutStr, Var varX])
           , (intern "getStr", Lambda [varX] $ Apply [Builtin GetStr])
           ]


varX = intern "x"

