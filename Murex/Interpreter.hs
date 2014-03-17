module Murex.Interpreter where

import Import
import Murex.Data
import Murex.Syntax.Typeless
import Murex.Interpreter.Values
import Murex.Interpreter.Builtin
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
    go (Literal x) = return (Data x)
    go (Prod xs) = error "TODO interpret products" --MurexRecord <$> (mapM evalElem xs)
        --where evalElem (l, x) = (,) l <$> go x
    go (Sum l x) = error "TODO interpret sums"
    go (List xs) = error "TODO interpret lists"
    go (Lambda xs e) = Closure xs e <$> getFindEnv
    go (Var x) = fromJust <$> Env.find x
    go (Define (Var x) e) = const (Data MurexUnit) <$> (Env.bind x =<< go e)
    go (Apply (Builtin f : args)) = liftIO . runBuiltin f =<< mapM go args
    go (Apply (f:args)) = flip apply args =<< go f
    go (Block [x]) = go x
    go (Block (x:xs)) = go x >> go (Block xs)
    go (LetIn def body) = letInEnv (go def) (go body)
    apply :: Value -> [AST] -> Interpreter Value
    apply (Closure xs e env) args = if numArgs >= numParams
        then do
            let (now, later) = splitAt numParams args
            now' <- mapM go now
            res <- withEnv env $ localEnv $ do
                mapM (uncurry bind) (zip xs now')
                go e
            if null later then return res else apply res later
        else do
            args' <- mapM go args
            let (now, later) = splitAt numArgs xs
            withEnv env $ localEnv $ do
                mapM (uncurry bind) (zip now args')
                Closure later e <$> getFindEnv
        where
        numArgs = length args
        numParams = length xs
    apply x args = error $ "got non-closure in apply: " ++ show x




