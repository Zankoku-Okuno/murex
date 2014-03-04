module Murex.Interpreter.Builtin where

import Import
import qualified Data.Sequence as S
import Data.Foldable (toList)
import System.IO
import Murex.Data
import Murex.Interpreter.Values
import Murex.Syntax.Typeless

runBuiltin :: Builtin -> [Value] -> IO Value
runBuiltin PutChr [Data (MurexChar c)] = putChar c >> return (Data MurexUnit)
runBuiltin GetChr [] = Data . MurexChar <$> getChar
runBuiltin PutStr [Data (MurexSeq str)] = do
    putStr $ map fromMurexChar (toList str)
    hFlush stdout
    return $ Data MurexUnit
runBuiltin GetStr [] = Data . MurexSeq . S.fromList . map toMurexChar <$> getLine

runBuiltin NotBool [Data a] = return $ Data $ notBool a
runBuiltin EqBool [Data a, Data b] = return $ Data $ eqBool a b
runBuiltin AndBool [Data a, Data b] = return $ Data $ andBool a b
runBuiltin OrBool [Data a, Data b] = return $ Data $ orBool a b
runBuiltin XorBool [Data a, Data b] = return $ Data $ xorBool a b
runBuiltin ElimBool [Data (MurexBool True), c, a] = return c
runBuiltin ElimBool [Data (MurexBool False), c, a] = return a

runBuiltin NegNum [Data a] = return $ Data $ negNum a
runBuiltin AddNum [Data a, Data b] = return $ Data $ addNum a b
runBuiltin SubNum [Data a, Data b] = return $ Data $ subNum a b
runBuiltin MulNum [Data a, Data b] = return $ Data $ mulNum a b
--runBuiltin QuoNum [Data a, Data b] = Data . fromJust <$> quoNum a b
--runBuiltin RemNum [Data a, Data b] = return $ Data $ remNum a b
--runBuiltin QuoremNum [Data a, Data b] = return $ Data $ quoremNum a b
runBuiltin DivNum [Data a, Data b] = return $ Data . fromJust $ divNum a b

runBuiltin EqNum [Data a, Data b] = return $ Data $ eqNum a b
runBuiltin NeqNum [Data a, Data b] = return $ Data $ neqNum a b
runBuiltin LtNum [Data a, Data b] = return $ Data $ ltNum a b
runBuiltin LteNum [Data a, Data b] = return $ Data $ lteNum a b
runBuiltin GtNum [Data a, Data b] = return $ Data $ gtNum a b
runBuiltin GteNum [Data a, Data b] = return $ Data $ gteNum a b

runBuiltin ConsSeq [Data x, Data xs] = return $ Data $ consSeq x xs
runBuiltin SnocSeq [Data xs, Data x] = return $ Data $ snocSeq xs x
runBuiltin CatSeq [Data a, Data b] = return $ Data $ catSeq a b
runBuiltin LenSeq [Data xs] = return $ Data $ lenSeq xs
--TODO seqIx, seqSet
--runBuiltin IxSeq [Data xs, Data i] = return $ Data . fromJust $ ixSeq xs i
runBuiltin HeadSeq [Data xs] = return $ Data . fromJust $ headSeq xs
runBuiltin TailSeq [Data xs] = return $ Data . fromJust $ tailSeq xs
runBuiltin InitSeq [Data xs] = return $ Data . fromJust $ initSeq xs
runBuiltin LastSeq [Data xs] = return $ Data . fromJust $ lastSeq xs

runBuiltin EqChr [Data a, Data b] = return $ Data $ eqChr a b

runBuiltin ChrNum [Data c] = return $ Data $ chrToNum c
--runBuiltin NumChr [Data n] = return $ Data $ numToChr n --TODO

startEnv :: AList Symbol AST
startEnv = [ (intern "putChr", Lambda [varX] $ Apply [Builtin PutChr, Var varX])
           , (intern "getChr", Lambda [varX] $ Apply [Builtin GetChr])
           , (intern "putStr", Lambda [varX] $ Apply [Builtin PutStr, Var varX])
           , (intern "getStr", Lambda [varX] $ Apply [Builtin GetStr])

           , (intern "notBool", Lambda [varX] $ Apply [Builtin NotBool, Var varX])
           , (intern "eqBool", Lambda [varX, varY] $ Apply [Builtin EqBool, Var varX, Var varY])
           , (intern "andBool", Lambda [varX, varY] $ Apply [Builtin AndBool, Var varX, Var varY])
           , (intern "orBool", Lambda [varX, varY] $ Apply [Builtin OrBool, Var varX, Var varY])
           , (intern "xorBool", Lambda [varX, varY] $ Apply [Builtin XorBool, Var varX, Var varY])
           , (intern "elimBool", Lambda [varX, varY, varZ] $ Apply [Builtin ElimBool, Var varX, Var varY, Var varZ])

           , (intern "negNum", Lambda [varX] $ Apply [Builtin NegNum, Var varX])
           , (intern "addNum", Lambda [varX, varY] $ Apply [Builtin AddNum, Var varX, Var varY])
           , (intern "subNum", Lambda [varX, varY] $ Apply [Builtin SubNum, Var varX, Var varY])
           , (intern "mulNum", Lambda [varX, varY] $ Apply [Builtin MulNum, Var varX, Var varY])
           , (intern "divNum", Lambda [varX, varY] $ Apply [Builtin DivNum, Var varX, Var varY])

           , (intern "eqNum", Lambda [varX, varY] $ Apply [Builtin EqNum, Var varX, Var varY])
           , (intern "neqNum", Lambda [varX, varY] $ Apply [Builtin NeqNum, Var varX, Var varY])
           , (intern "ltNum", Lambda [varX, varY] $ Apply [Builtin LtNum, Var varX, Var varY])
           , (intern "lteNum", Lambda [varX, varY] $ Apply [Builtin LteNum, Var varX, Var varY])
           , (intern "gtNum", Lambda [varX, varY] $ Apply [Builtin GtNum, Var varX, Var varY])
           , (intern "gteNum", Lambda [varX, varY] $ Apply [Builtin GteNum, Var varX, Var varY])

           , (intern "eqChr", Lambda [varX, varY] $ Apply [Builtin EqChr, Var varX, Var varY])

           , (intern "len", Lambda [varX] $ Apply [Builtin LenSeq, Var varX])
           , (intern "cons", Lambda [varX, varY] $ Apply [Builtin ConsSeq, Var varX, Var varY])
           , (intern "snoc", Lambda [varX, varY] $ Apply [Builtin SnocSeq, Var varX, Var varY])
           , (intern "cat", Lambda [varX, varY] $ Apply [Builtin CatSeq, Var varX, Var varY])
           , (intern "hd", Lambda [varX] $ Apply [Builtin HeadSeq, Var varX])
           , (intern "tl", Lambda [varX] $ Apply [Builtin TailSeq, Var varX])
           , (intern "last", Lambda [varX] $ Apply [Builtin LastSeq, Var varX])
           , (intern "init", Lambda [varX] $ Apply [Builtin InitSeq, Var varX])
           
           , (intern "num->chr", Lambda [varX] $ Apply [Builtin NumChr, Var varX])
           , (intern "chr->num", Lambda [varX] $ Apply [Builtin ChrNum, Var varX])
           ]




varX = intern "x"
varY = intern "y"
varZ = intern "z"

