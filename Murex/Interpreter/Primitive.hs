module Murex.Interpreter.Primitive (
	  startEnv
	, runBuiltin
	) where

import Import
import Murex.Interpreter.Values
import Murex.Interpreter.Builtin
import Murex.Syntax.Abstract (AST(..), Builtin(..))
import System.IO
import Data.Foldable (toList)
import qualified Data.Sequence as S


------ Perform Primitives ------
runBuiltin :: Builtin -> [Value] -> IO Value
runBuiltin PutChr [MurexChar c] = putChar c >> return MurexUnit
runBuiltin GetChr [] = MurexChar <$> getChar
runBuiltin PutStr [MurexSeq str] = do
    putStr $ map fromMurexChar (toList str)
    hFlush stdout
    return $ MurexUnit
runBuiltin GetStr [] = MurexSeq . S.fromList . map toMurexChar <$> getLine

runBuiltin NotBool [a] = return $ notBool a
runBuiltin EqBool [a, b] = return $ eqBool a b
runBuiltin AndBool [a, b] = return $ andBool a b
runBuiltin OrBool [a, b] = return $ orBool a b
runBuiltin XorBool [a, b] = return $ xorBool a b
runBuiltin ElimBool [MurexBool True, c, a] = return c
runBuiltin ElimBool [MurexBool False, c, a] = return a

runBuiltin NegNum [a] = return $ negNum a
runBuiltin AddNum [a, b] = return $ addNum a b
runBuiltin SubNum [a, b] = return $ subNum a b
runBuiltin MulNum [a, b] = return $ mulNum a b
--runBuiltin QuoNum [a, b] = fromJust <$> quoNum a b
--runBuiltin RemNum [a, b] = return $ remNum a b
--runBuiltin QuoremNum [a, b] = return $ quoremNum a b
runBuiltin DivNum [a, b] = return . fromJust $ divNum a b

runBuiltin EqNum [a, b] = return $ eqNum a b
runBuiltin NeqNum [a, b] = return $ neqNum a b
runBuiltin LtNum [a, b] = return $ ltNum a b
runBuiltin LteNum [a, b] = return $ lteNum a b
runBuiltin GtNum [a, b] = return $ gtNum a b
runBuiltin GteNum [a, b] = return $ gteNum a b

runBuiltin ConsSeq [x, xs] = return $ consSeq x xs
runBuiltin SnocSeq [xs, x] = return $ snocSeq xs x
runBuiltin CatSeq [a, b] = return $ catSeq a b
runBuiltin LenSeq [xs] = return $ lenSeq xs
--TODO seqIx, seqSet
--runBuiltin IxSeq [xs, i] = return . fromJust $ ixSeq xs i
runBuiltin HeadSeq [xs] = return . fromJust $ headSeq xs
runBuiltin TailSeq [xs] = return . fromJust $ tailSeq xs
runBuiltin InitSeq [xs] = return . fromJust $ initSeq xs
runBuiltin LastSeq [xs] = return . fromJust $ lastSeq xs

runBuiltin EqChr [a, b] = return $ eqChr a b

runBuiltin (ProjFn l) [x] = return . fromJust $ project l x

runBuiltin ChrNum [c] = return $ chrToNum c
--runBuiltin NumChr [n] = return $ numToChr n --TODO


------ Expose Primitives ------
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
