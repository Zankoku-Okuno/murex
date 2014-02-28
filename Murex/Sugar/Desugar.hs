module Murex.Sugar.Desugar where

import Import
import Murex.Sugar
import Data.Hierarchy
import Data.Hexpr
import Murex.Syntax.Concrete
import Murex.Sugar.Notation
import Control.Monad.Errors
import Text.Parsec (ParseError)
import qualified Text.Parsec as P

desugar :: [NotationConfig] -> Tree -> Either [ParseError] Tree
desugar notation input = runDesugar anyNode $ transKw defaultKeywords input

{-
What I need is to input a tree.
    Perform some desugaring at that node.
    Validate, on fail return just the desugared node.
    Successful validation desugars underneath and returns the tree.
    Desugaring does not muck with positioning of existing structures.
I think what I need is a ParsecT [Tree] () (Errors [ParseError]) Tree
-}

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
    block <- leaf (Prim Block)
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