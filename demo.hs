import Import
import Murex.Interpreter
import qualified Murex.Syntax.Lexer as Lex
import qualified Murex.Syntax.Parser as Par

import qualified Data.Sequence as S
import Murex.Data
import Murex.Interpreter.Builtin
import Murex.Syntax.Typeless

hello = putStrLn "Mesdames, messieures, bon soir!"
goodbye = putStrLn "Goodbyte, cruel world!"

main = do
    hello
    runEitherT $ do
        tokens <- case Lex.runLexer "demo" normalTest of
            Left err -> left () <* liftIO (print err)
            Right tokens -> return tokens <* liftIO (print $ map snd tokens)
        trees <- case Par.runParser tokens of
            Left err -> liftIO (print err) >> left ()
            Right trees -> void $ liftIO $ mapM_ print trees
    --interpret $ Apply [Var (intern "putStr"),
    --                Apply [Var (intern "snoc"),
    --                    Apply [Var (intern "getStr"), Literal MurexUnit],
    --                    Literal (MurexChar '\n')]]
    --print =<< interpret (Apply [Apply [murexIgnore, Literal (MurexNum (1%1))], Literal (MurexNum (2%1))])
    goodbye

murexConst = Lambda [intern "x", intern "y"] (Var $ intern "x")
murexIgnore = Lambda [intern "x", intern "y"] (Var $ intern "y")

normalTest = "'a' ()\n\
             \   lambda\n\
             \   `body `1\n\
             \3/5\n\
             \.(@`foo a.b.c)"
indentTest = "\n \n#hi\n (\\\n\n)\n      \n  \\\\\n    ()\n ()\n#asgf"