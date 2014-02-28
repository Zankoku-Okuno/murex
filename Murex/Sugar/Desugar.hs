module Murex.Sugar.Desugar where

import Import
import Murex.Sugar
import Data.Hierarchy
import Data.Hexpr
import Language.Desugar
import Murex.Syntax.Concrete
import Murex.Sugar.Notation
import Control.Monad.Errors
import Text.Parsec (ParseError)
import qualified Text.Parsec as P

desugar :: [NotationConfig] -> Tree -> Either [ParseError] Tree
desugar notation input = runDesugar anyNode $ implicitParens $ transKw defaultKeywords input


expression :: Desugar Tree
expression = subNode $ P.choice [ block
                                , lambda
                                , P.many1 expression
                                ]

binder :: Desugar Tree
binder = subNode $ (:[]) <$> shortId


------ Special Forms ------
--FIXME accept more than only expressions
block :: Desugar [Tree]
block = do
    block <- leaf (Prim TopLevel) -- <|> leaf (Prim Block)
    body <- P.many1 expression
    return (block:body)

lambda :: Desugar [Tree]
lambda = do
        lambda <- leaf (Kw Lambda)
        binding <- subNode $ P.many1 binder
        body <- P.many1 expression
        return [lambda, binding, adjoinsPos body]

apply :: Desugar [Tree]
apply = P.many1 anyNode


------ Keywords ------
transKw :: AList String Keyword -> Tree -> Tree
transKw config = postorder (go, id)
    where
    go (Name [x]) = case lookup x config of
        Nothing -> Name [x]
        Just kw -> Kw kw
    go x = x

defaultKeywords :: AList String Keyword
defaultKeywords = [ ("λ", Lambda)
                  ]

------ Parens ------
implicitParens :: Tree -> Tree
implicitParens = addParens isLambda
    where
    isLambda (QLeaf _ (Kw Lambda)) = True
    isLambda _ = False