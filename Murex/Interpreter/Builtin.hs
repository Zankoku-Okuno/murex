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

runBuiltin Not [Data a] = return $ Data $ notBool a
runBuiltin EqBool [Data a, Data b] = return $ Data $ eqBool a b
runBuiltin And [Data a, Data b] = return $ Data $ andBool a b
runBuiltin Or [Data a, Data b] = return $ Data $ orBool a b
runBuiltin Xor [Data a, Data b] = return $ Data $ xorBool a b

runBuiltin NegNum [Data a] = return $ Data $ negNum a
runBuiltin AddNum [Data a, Data b] = return $ Data $ addNum a b
runBuiltin SubNum [Data a, Data b] = return $ Data $ subNum a b
runBuiltin MulNum [Data a, Data b] = return $ Data $ mulNum a b
--runBuiltin QuoNum [Data a, Data b] = return $ Data $ quoNum a b
--runBuiltin RemNum [Data a, Data b] = return $ Data $ remNum a b
--runBuiltin QuoremNum [Data a, Data b] = return $ Data $ quoremNum a b
--runBuiltin DivNum [Data a, Data b] = return $ Data $ divNum a b

runBuiltin EqNum [Data a, Data b] = return $ Data $ eqNum a b
runBuiltin Neq [Data a, Data b] = return $ Data $ neqNum a b
runBuiltin Lt [Data a, Data b] = return $ Data $ ltNum a b
runBuiltin Lte [Data a, Data b] = return $ Data $ lteNum a b
runBuiltin Gt [Data a, Data b] = return $ Data $ gtNum a b
runBuiltin Gte [Data a, Data b] = return $ Data $ gteNum a b

runBuiltin Cons [Data x, Data xs] = return $ Data $ seqCons x xs
runBuiltin Snoc [Data xs, Data x] = return $ Data $ seqSnoc xs x
runBuiltin Cat [Data a, Data b] = return $ Data $ seqCat a b
runBuiltin SeqLen [Data xs] = return $ Data $ seqLen xs
--TODO seqIx, seqSet
--TODO hd, tl, last, init

runBuiltin ChrNum [Data c] = return $ Data $ chrToNum c
--runBuiltin NumChr [Data n] = return $ Data $ numToChr n --TODO

startEnv :: AList Symbol AST
startEnv = [ (intern "putChr", Lambda [varX] $ Apply [Builtin PutChr, Var varX])
           , (intern "getChr", Lambda [varX] $ Apply [Builtin GetChr])
           , (intern "putStr", Lambda [varX] $ Apply [Builtin PutStr, Var varX])
           , (intern "getStr", Lambda [varX] $ Apply [Builtin GetStr])

           , (intern "notBool", Lambda [varX] $ Apply [Builtin Not, Var varX])
           , (intern "eqBool", Lambda [varX, varY] $ Apply [Builtin EqBool, Var varX, Var varY])
           , (intern "andBool", Lambda [varX, varY] $ Apply [Builtin And, Var varX, Var varY])
           , (intern "orBool", Lambda [varX, varY] $ Apply [Builtin Or, Var varX, Var varY])
           , (intern "xorBool", Lambda [varX, varY] $ Apply [Builtin Xor, Var varX, Var varY])

           , (intern "negNum", Lambda [varX] $ Apply [Builtin NegNum, Var varX])
           , (intern "addNum", Lambda [varX, varY] $ Apply [Builtin AddNum, Var varX, Var varY])
           , (intern "subNum", Lambda [varX, varY] $ Apply [Builtin SubNum, Var varX, Var varY])
           , (intern "mulNum", Lambda [varX, varY] $ Apply [Builtin MulNum, Var varX, Var varY])
           , (intern "divNum", Lambda [varX, varY] $ Apply [Builtin DivNum, Var varX, Var varY])

           , (intern "eqNum", Lambda [varX, varY] $ Apply [Builtin EqNum, Var varX, Var varY])
           , (intern "neqNum", Lambda [varX, varY] $ Apply [Builtin Neq, Var varX, Var varY])
           , (intern "ltNum", Lambda [varX, varY] $ Apply [Builtin Lt, Var varX, Var varY])
           , (intern "lteNum", Lambda [varX, varY] $ Apply [Builtin Lte, Var varX, Var varY])
           , (intern "gtNum", Lambda [varX, varY] $ Apply [Builtin Gt, Var varX, Var varY])
           , (intern "gteNum", Lambda [varX, varY] $ Apply [Builtin Gte, Var varX, Var varY])

           , (intern "len", Lambda [varX] $ Apply [Builtin Cons, Var varX])
           , (intern "cons", Lambda [varX, varY] $ Apply [Builtin Cons, Var varX, Var varY])
           , (intern "snoc", Lambda [varX, varY] $ Apply [Builtin Snoc, Var varX, Var varY])
           , (intern "cat", Lambda [varX, varY] $ Apply [Builtin Cat, Var varX, Var varY])
           , (intern "hd", Lambda [varX] $ Apply [Builtin Head, Var varX])
           , (intern "tl", Lambda [varX] $ Apply [Builtin Tail, Var varX])
           , (intern "last", Lambda [varX] $ Apply [Builtin Last, Var varX])
           , (intern "init", Lambda [varX] $ Apply [Builtin Init, Var varX])
           
           , (intern "num->chr", Lambda [varX] $ Apply [Builtin NumChr, Var varX])
           , (intern "chr->num", Lambda [varX] $ Apply [Builtin ChrNum, Var varX])
           ]




varX = intern "x"
varY = intern "y"

