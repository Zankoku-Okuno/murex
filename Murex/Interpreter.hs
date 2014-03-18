module Murex.Interpreter where

import Import hiding (find)
import qualified Data.Sequence as S
import Murex.Syntax.Abstract (AST(..))
import Murex.Interpreter.Values
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
    apply x args = error $ "got non-applicable in apply: " ++ show x




