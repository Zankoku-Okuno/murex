import Import
import Murex.Interpreter
import Control.Monad.Errors
import qualified Murex.Lexer as Lex
import qualified Murex.Parser as Par
import qualified Murex.Sugar.Notation as Notation
import qualified Murex.Sugar.Desugar as Desugar
import qualified Murex.Syntax.Concrete as Concrete

import qualified Data.Sequence as S
import Murex.Data
import Murex.Interpreter.Builtin
import Murex.Syntax.Typeless

hello = putStrLn "Mesdames, messieures, bon soir!"
goodbye = putStrLn "Goodbyte, cruel world!"

main = do
    hello
    let input = interpTest
    runEitherT $ do
        sep
        liftIO $ putStrLn input
        sep
        tokens <- case Lex.runLexer "demo" tinyTest of
            Left err -> liftIO (print err) *> left()
            Right tokens -> return tokens
        liftIO $ print $ map snd tokens
        sep
        trees <- case Par.runParser tokens of
            Left err -> liftIO (print err) *> left()
            Right val -> return val
        (notation, raw) <- case runErrors (Notation.extractNotation "demo" trees) of
            Left errs -> liftIO (mapM_ print errs) *> left()
            Right val -> return val
        liftIO $ mapM_ print notation
        sep
        desugared <- case Desugar.desugar notation raw of
            Left err -> liftIO (print err) *> left()
            Right val -> return val
        liftIO $ print desugared
        sep
        let asts = Concrete.toAST desugared
        liftIO $ print asts
        sep
        results <- liftIO $ interpret asts
        liftIO $ print results
        sep
    goodbye
    where
    sep = liftIO $ putStrLn (replicate 36 '=')

murexConst = Lambda [intern "x", intern "y"] (Var $ intern "x")
murexIgnore = Lambda [intern "x", intern "y"] (Var $ intern "y")

tinyTest = "3"
interpTest = "putStr \">\"\n\
             \putStr\n   snoc (getStr ()) '\\n'\n\
             \(λ (f x) f (f x)) (λ x addNum x x) 3"
echoTest = ""
tokenTest = "'a' ()\n\
             \   lambda\n\
             \   `body `1\n\
             \3/5\n\
             \.(@`foo a.b.c)"
indentTest = "\n \n#hi\n (\\\n\n)\n      \n  \\\\\n    ()\n ()\n#asgf"