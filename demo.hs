import Import
import Murex.Interpreter
import qualified Murex.Syntax.Lexer as Lex

import qualified Data.Sequence as S
import Murex.Data
import Murex.Interpreter.Builtin
import Murex.Syntax.Typeless

hello = putStrLn "Mesdames, messieures, bon soir!"
goodbye = putStrLn "Goodbyte, cruel world!"

main = do
    hello
    case Lex.runLexer "demo" normalTest of
        Left err -> print err
        Right val -> print (map snd val)
    --interpret $ Apply [Var (intern "putStr"),
    --                Apply [Var (intern "snoc"),
    --                    Apply [Var (intern "getStr"), Literal MurexUnit],
    --                    Literal (MurexChar '\n')]]
    --print =<< interpret (Apply [Apply [murexIgnore, Literal (MurexNum (1%1))], Literal (MurexNum (2%1))])
    goodbye

murexConst = Lambda [intern "x", intern "y"] (Var $ intern "x")
murexIgnore = Lambda [intern "x", intern "y"] (Var $ intern "y")

normalTest = "'a'\n\
             \   lambda\n\
             \   `body `1\n\
             \3/5"
indentTest = "\n \n#hi\n (\\\n\n)\n      \n  ()\n ()\n#asgf"