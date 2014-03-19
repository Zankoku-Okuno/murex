module Murex.Interpreter (interpret) where

import Import hiding (find)
import qualified Data.Sequence as S
import Murex.Syntax.Abstract (AST(..))
import Murex.Interpreter.Values
import Murex.Interpreter.Builtin (project, update)
import Murex.Interpreter.Primitive
import Control.Monad.Environment
import qualified Control.Monad.Environment as Env


type Interpreter = EnvironmentIO Symbol Value

interpret :: AST -> IO Value
interpret ast = evalEnvironmentT [] $ do
        startEnv' <- mapM evalBuiltins startEnv
        mapM (uncurry bind) startEnv'
        go ast
    where
    evalBuiltins (x, e) = (,) x <$> go e

go :: AST -> Interpreter Value
go (Lit x) = return (toValue x)
go (Sequence xs) = MurexSeq . S.fromList <$> (mapM go xs)
go (Record xs) = MurexRecord <$> (mapM evalElem xs)
    where evalElem (l, x) = (,) l <$> go x
go (Variant l x) = MurexVariant l <$> go x
go (Lambda xs e) = Closure xs e <$> getFindEnv
go (Var x) = fromJust <$> find x
go (Define (Var x) e) = const MurexUnit <$> (bind x =<< go e)
go (Apply (Builtin f : args)) = liftIO . runBuiltin f =<< mapM go args
go (Apply (f:args)) = flip apply args =<< go f
    where
    apply f args = do
        let (now, later) = splitAt (numParams f) args
        now' <- mapM go now
        res <- applyVals f now'
        if null later then return res else apply res later
go (Block [x]) = go x
go (Block (x:xs)) = go x >> go (Block xs)
go (LetIn def body) = letInEnv (go def) (go body)
go (Project l) = return $ PrjFn l
go (Modify l f) = ModFn l <$> go f
go (Update l f) = UpdFn l <$> go f
go x = error $ "Murex.Interpreter.go: " ++ show x


numParams :: Value -> Int
numParams (Closure xs _ _) = length xs
numParams (PrjFn _) = 1
numParams (ModFn _ _) = 1
numParams (UpdFn _ _) = 1
numParams x = error $ "Murex.Interpreter.numParams: " ++ show x

applyVals :: Value -> [Value] -> Interpreter Value
applyVals (Closure xs e env) args = withEnv env $ localEnv $
    if numParams > length args
    then do
        mapM (uncurry bind) (zip xs args)
        Closure (drop numArgs xs) e <$> getFindEnv
    else do
        mapM (uncurry bind) (zip xs args)
        go e
    where
    numParams = length xs
    numArgs = length args
applyVals (PrjFn l) [arg] = return . fromJust $ project l arg
applyVals (ModFn l f) [arg] = do
    let old = fromJust $ project l arg
    new <- applyVals f [old]
    return . fromJust $ update l arg new
applyVals (UpdFn l x) [arg] =
    return . fromJust $ update l arg x
applyVals x _ = error $ "Murex.Interpreter.applyVals: " ++ show x





