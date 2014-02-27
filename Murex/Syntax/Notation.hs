module Murex.Syntax.Notation where

import Import
import Data.Hierarchy
import Data.Hexpr
import Language.Distfix
import Control.Monad.Errors
import qualified Text.Parsec as P
import Text.Parsec (Parsec, ParseError, SourcePos, token)
import Murex.Parser

data NotationConfig = Use    SourcePos String
                    | Define SourcePos String [DistfixLevel]
                    -- TODO maybe ability to configure keywords
    deriving (Show)
data DistfixLevel = AnonLevel  SourcePos [Distfix (Hexpr SourcePos Atom)]
                  | NamedLevel SourcePos String
    deriving (Show)

type Desugar a = Parsec [Quasihexpr SourcePos Atom] () a
type Tree = Quasihexpr SourcePos Atom

extractNotation :: [Tree] -> Errors [ParseError] ([NotationConfig], [Tree])
extractNotation input = let (directives, body) = partition isNotationDirective input
                        in flip (,) body . catMaybes <$> mapRecover (map parseNotation directives)
    where
    isNotationDirective (QLeaf _ (Name ["notation"])) = True
    isNotationDirective (QBranch _ (QLeaf _ (Name ["notation"]):_)) = True
    isNotationDirective _ = False

parseNotation :: Tree -> Errors [ParseError] NotationConfig
parseNotation = hoistParsec $ do
    word "notation"
    error "TODO"


hoistParsec :: Desugar a -> Tree -> Errors [ParseError] a
hoistParsec parser input = hoistEither1 $ runParser "" $ case input of
        QBranch _ xs -> xs
        x -> [x]
    where
    runParser = P.parse $ do
        P.getInput >>= \input -> case input of { [] -> return (); (x:xs) -> P.setPosition (getPos x) }
        res <- parser
        remaining <- P.getInput
        case remaining of { [] -> return res; _ -> fail "unexpected end of node" }

word :: String -> Desugar Tree
word name = token show getPos checkWord
    where
    checkWord node@(QLeaf _ (Name [name'])) | name == name' = Just node
    checkWord _ = Nothing




