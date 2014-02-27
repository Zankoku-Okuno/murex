import Import
import Murex.Interpreter
import Control.Monad.Errors
import qualified Murex.Lexer as Lex
import qualified Murex.Parser as Par
import qualified Murex.Syntax.Notation as Notation
import qualified Murex.Syntax.Desugar as Desugar
import qualified Murex.Syntax.Concrete as Concrete

import qualified Data.Sequence as S
import Murex.Data
import Murex.Interpreter.Builtin
import Murex.Syntax.Typeless

hello = putStrLn "Mesdames, messieures, bon soir!"
goodbye = putStrLn "Goodbyte, cruel world!"

main = do
    hello
    runEitherT $ do
        sep
        tokens <- case Lex.runLexer "demo" interpTest of
            Left err -> liftIO (print err) *> left()
            Right tokens -> return tokens
        liftIO $ print $ map snd tokens
        sep
        trees <- case Par.runParser tokens of
            Left err -> liftIO (print err) *> left()
            Right val -> return val
        (notation, raw) <- case runErrors (Notation.extractNotation trees) of
            Left errs -> liftIO (mapM_ print errs) *> left()
            Right val -> return val
        liftIO $ mapM_ print notation
        sep
        desugared <- case Desugar.desugar notation (head raw) of
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
    --interpret $ Apply [Var (intern "putStr"),
    --                Apply [Var (intern "snoc"),
    --                    Apply [Var (intern "getStr"), Literal MurexUnit],
    --                    Literal (MurexChar '\n')]]
    goodbye
    where
    sep = liftIO $ putStrLn (replicate 36 '=')

murexConst = Lambda [intern "x", intern "y"] (Var $ intern "x")
murexIgnore = Lambda [intern "x", intern "y"] (Var $ intern "y")

interpTest = "(λ (f x) f (f x)) (λ x addNum x x) 3"
tokenTest = "'a' ()\n\
             \   lambda\n\
             \   `body `1\n\
             \3/5\n\
             \.(@`foo a.b.c)"
indentTest = "\n \n#hi\n (\\\n\n)\n      \n  \\\\\n    ()\n ()\n#asgf"