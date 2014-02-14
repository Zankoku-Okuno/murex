import Import
import Murex.Interpreter

import qualified Data.Sequence as S
import Murex.Data
import Murex.Syntax.Typeless

hello = putStrLn "Mesdames, messieures, bon soir!"
goodbye = putStrLn "Goodbyte, cruel world!"

main = do
	hello
	print =<< interpret (Apply [Apply [murexIgnore, Literal (MurexNum (1%1))], Literal (MurexNum (2%1))])
	--(Literal (MurexSeq $ S.fromList [MurexNum (-42%137), MurexUnit]))
	goodbye

murexConst = Lambda [intern "x", intern "y"] (Var $ intern "x")
murexIgnore = Lambda [intern "x", intern "y"] (Var $ intern "y")