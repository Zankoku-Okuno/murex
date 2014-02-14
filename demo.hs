import Import
import Murex.Interpreter

import qualified Data.Sequence as S
import Murex.Data
import Murex.Interpreter.Builtin
import Murex.Syntax.Typeless

hello = putStrLn "Mesdames, messieures, bon soir!"
goodbye = putStrLn "Goodbyte, cruel world!"

main = do
	hello
	interpret $ Apply [murexIgnore,
						(Apply [Var (intern "putStr"), Apply [Var (intern "getStr"), Literal MurexUnit]]),
						(Apply [Var (intern "putChr"), Literal (MurexChar '\n')])
					]
	print =<< interpret (Apply [Apply [murexIgnore, Literal (MurexNum (1%1))], Literal (MurexNum (2%1))])
	goodbye

murexConst = Lambda [intern "x", intern "y"] (Var $ intern "x")
murexIgnore = Lambda [intern "x", intern "y"] (Var $ intern "y")