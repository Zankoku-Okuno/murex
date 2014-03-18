module Murex.Sugar.Notation where

import Import
import Data.Hierarchy
import Data.Hexpr
import Language.Distfix
import Control.Monad.Errors
import qualified Text.Parsec as P
import Text.Parsec (ParseError, SourceName)
import Text.Parsec.Pos (newPos)
import Murex.Syntax.Concrete
import Murex.Parser

data NotationConfig = Use    SourcePos String
                    | Define SourcePos String [DistfixLevel]
                    -- TODO maybe ability to configure keywords
    deriving (Show)
data DistfixLevel = AnonLevel  SourcePos [Distfix (Hexpr SourcePos Atom)]
                  | NamedLevel SourcePos String
    deriving (Show)


extractNotation :: SourceName -> [Tree] -> Errors [ParseError] ([NotationConfig], Tree)
extractNotation sourcename input = let (directives, body) = partition isNotationDirective input
                                   in (,) <$> transNotation directives <*> pure (transBody body)
    where
    isNotationDirective (QLeaf _ (Name ["notation"])) = True
    isNotationDirective (QBranch _ (QLeaf _ (Name ["notation"]):_)) = True
    isNotationDirective _ = False
    transBody [body] = body
    transBody body = individual (newPos sourcename 1 1) (Kw Block) `adjoinslPos` body
    transNotation directives = catMaybes <$> mapRecover (map parseNotation directives)

parseNotation :: Tree -> Errors [ParseError] NotationConfig
parseNotation = error "TODO"




