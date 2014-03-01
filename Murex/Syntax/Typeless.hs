module Murex.Syntax.Typeless where

import Import
import Murex.Data

data AST = Literal MurexData
         | Var Symbol
         | Define AST AST
         | Lambda [Symbol] AST
         | Apply [AST]
         | Block [AST]
         | LetIn AST AST
         | Builtin Builtin
    deriving (Show)