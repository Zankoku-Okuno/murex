module Murex.Syntax.Typeless where

import Import
import Murex.Data

data AST = Literal MurexData
         | Var Symbol
         | Lambda [Symbol] AST
         | Apply [AST]