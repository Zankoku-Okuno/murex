import Import
import Murex.Interpreter
import Control.Monad.Errors (runErrors)
import qualified Murex.Lexer as Lex
import qualified Murex.Parser as Par
import qualified Murex.Sugar.Notation as Notation
import qualified Murex.Sugar.Desugar as Desugar
import qualified Murex.Syntax.Concrete as Concrete

import qualified Data.Sequence as S
import Murex.Interpreter.Builtin
import Murex.Syntax.Abstract

import System.Environment

hello = putStrLn "Mesdames, messieures, bon soir!"
goodbye = putStrLn "Goodbyte, cruel world!"


problemTest = "(1 2 3"

interpTest = "putStr \">\"\n\
             \putStr\n   snoc (getStr ()) '\\n'\n\
             \(λ (f x) f (f x)) (λ x addNum x x) 3"
tokenTest = "'a' ()\n\
             \   lambda\n\
             \   `body `1\n\
             \3/5\n\
             \.(@`foo a.b.c)"
indentTest = "\n \n#hi\n (\\\n\n)\n      \n  \\\\\n    ()\n ()\n#asgf"


main = do
    args <- getArgs
    input <- case args of 
        [filename] -> readFile filename
        _ -> error "bad arguments"
    hello
    runEitherT $ do
        sep "tokens"
        tokens <- case Lex.runLexer "demo" input of
            Left err -> liftIO (print err) *> left ()
            Right tokens -> return tokens
        liftIO $ print $ map snd tokens
        sep "notation"
        trees <- case Par.runParser tokens of
            Left err -> liftIO (print err) *> left ()
            Right val -> return val
        (notation, raw) <- case runErrors (Notation.extractNotation "demo" trees) of
            Left errs -> liftIO (mapM_ print errs) *> left ()
            Right val -> return val
        liftIO $ mapM_ print notation
        sep "distfixes"
        (distfixExport, distfixTable) <- case runErrors $ Notation.interpretNotation notation of
            Left errs -> liftIO (mapM_ print errs) *> left ()
            Right val -> right val
        liftIO $ mapM_ print distfixTable
        sep "raw"
        liftIO $ print raw
        sep "cannonize"
        let cannon = Desugar.cannonize . Desugar.detectKeywords notation $ raw
        liftIO $ print cannon
        --TODO anon points
        let noAnon = cannon
        --TODO de-distfix
        sep "de-distfix"
        dedistfixed <- case Desugar.dedistfix distfixTable noAnon of
            Left err -> liftIO (print err) *> left ()
            Right val -> right val
        --TODO dotted expressions
        dotexprs <- case Desugar.dotExprs dedistfixed of
            Left err -> liftIO (print err) *> left ()
            Right val -> right val
        liftIO $ print dotexprs
        --TODO unquasiquote
        let desugared = dotexprs
        --FIXME check syntax instead of using that crappy Concrete.toAST function
        sep "ast"
        let ast = Concrete.toAST desugared
        liftIO $ print ast
        sep "eval"
        results <- liftIO $ interpret ast
        liftIO $ print results
        sep "done"
    goodbye
    where
    sep stage = liftIO $ putStrLn (replicate 36 '=' ++ " " ++ stage)
