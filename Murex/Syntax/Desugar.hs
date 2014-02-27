module Murex.Syntax.Desugar where

import Import
import Data.Hierarchy
import Data.Hexpr
import Murex.Syntax.Concrete
import Murex.Syntax.Notation
import Control.Monad.Errors

desugar :: [NotationConfig] -> Quasihexpr SourcePos Atom -> Either String (Quasihexpr SourcePos Atom)
desugar notation input = Right $ postorder (transKw, id) input
	where
	transKw (Name ["\955"]) = Kw Lambda
	transKw x = x

