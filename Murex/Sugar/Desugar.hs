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

specialForms :: [NotationConfig] -> Tree -> Tree
specialForms notation = implicitParens . transKw defaultKeywords

desugar :: Tree -> Either [ParseError] Tree
desugar = runDesugar expression

dedistfix :: [NotationConfig] -> Tree -> Tree
dedistfix notation = error "TODO"


definition :: Desugar Tree
definition = P.choice [ expression ]

expression :: Desugar Tree
expression = subNode $ do
    input <- P.getInput
    case input of 
        [_] -> (:[]) <$> satisfy isAtom
        _ -> P.choice [ block
                      , letin
                      , lambda
                      , defVal
                      , (:) <$> expression <*> P.many1 expression
                      ]
    where
    isAtom (QLeaf _ (Kw _)) = False
    isAtom (QLeaf _ _) = True
    isAtom _ = False

binder :: Desugar Tree
binder = subNode $ (:[]) <$> shortId


------ Special Forms ------
letin :: Desugar [Tree]
letin = do
    letkw <- leaf (Kw Let)
    body <- subNode $ do
        defs <- (:) <$> definition <*> definition `P.manyTill` P.lookAhead (leaf $ Kw In)
        inkw <- leaf (Kw In)
        body <- P.many1 definition
        let defs' = case defs of
                        [x] -> x
                        xs -> (QLeaf (getPos letkw) (Kw Block)) `adjoinslPos` defs
            body' = case body of
                        [x] -> x
                        xs -> (QLeaf (getPos inkw) (Kw Block)) `adjoinslPos` body
        return [defs', inkw, body']
    return [letkw, body]

--FIXME accept more than only expressions
block :: Desugar [Tree]
block = do
    block <- leaf (Kw Block)
    body <- P.many1 expression
    return (block:body)

lambda :: Desugar [Tree]
lambda = do
    lambda <- leaf (Kw Lambda)
    binding <- subNode $ P.many1 binder
    body <- P.many1 expression
    return [lambda, binding, adjoinsPos body]

defVal :: Desugar [Tree]
defVal = do
    def <- leaf (Kw Def)
    binding <- binder
    body <- expression
    return [def, binding, body]


------ Keywords ------
transKw :: AList String Keyword -> Tree -> Tree
transKw config = postorder (go, id)
    where
    go (Name [x]) = case lookup x config of
        Nothing -> Name [x]
        Just kw -> Kw kw
    go x = x

defaultKeywords :: AList String Keyword
defaultKeywords = [ ("λ",   Lambda)
                  , ("let", Let)
                  , ("in",  In)
                  , ("≡",   Def)
                  ]

------ Parens ------
implicitParens :: Tree -> Tree
implicitParens = preorder (leftInfix isDef) . preorder (addParens isLambdy)
--TODO \l &co first, then \def, then :
--TODO (type/macro) name \def body
    where
    isLambdy (QLeaf _ (Kw kw)) = case kw of
        Lambda -> True
        _ -> False
    isLambdy _ = False
    isDef (QLeaf _ (Kw Def)) = True
    isDef _ = False