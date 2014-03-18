module Murex.Sugar.Desugar where

import Import
import Data.Hierarchy
import Data.Hexpr
import Language.Desugar
import Murex.Syntax.Concrete
import Murex.Sugar.Notation
import Control.Monad.Errors
import Text.Parsec (ParseError)
import qualified Text.Parsec as P

detectKeywords :: [NotationConfig] -> Tree -> Tree
detectKeywords config = preorder (go keywords, id)
    where
    keywords = defaultKeywords --STUB
    go keywords (Name [x]) = case x `lookup` keywords of
        Nothing -> Name [x]
        Just kw -> Kw kw
    go keywords x = x



cannonize :: Tree -> Tree
cannonize input = let atSymbols = preorder (id, rewriteAtSign) input
                      defHeaders = atSymbols --STUB
                      lambdaesque = preorder (id, rewriteLambdaesque) defHeaders
                      letIn = preorder (id, rewriteLet) lambdaesque
                      infixes = preorder (forwardInfix (`kwIs` Def)) letIn --STUB
                  in infixes

dedistfix :: [NotationConfig] -> Tree -> Tree
dedistfix notation = error "TODO"


------ Keywords ------
defaultKeywords :: AList String Keyword
defaultKeywords = [ ("λ",   Lambda)
                  , ("let", Let)
                  , ("in",  In)
                  , ("≡",   Def)
                  --TODO :, type, data, macro, api, module
                  ]

------ Cannonize ------
rewriteAtSign :: [Tree] -> [Tree]
rewriteAtSign = tripBy (`kwIs` At) (id, go)
    where
    go before at after = before ++ case after of
        (x:xs) | isName x -> error "STUB: alias pattern"
               | isLabel x -> (QLeaf (getPos at) (Prim Project) `adjoinPos` x) : xs
               | isNode x -> error "STUB: update or modify"
        _ -> at:after
    unLabel (Label name) = name

rewriteLambdaesque :: [Tree] -> [Tree]
rewriteLambdaesque = tripBy isLambdaesque (id, go)
    where
    isLambdaesque = (`kwElem` [Lambda]) --TODO big lambda, forall
    go before l after = case after of
        (x:e@(_:_)) -> before ++ [adjoinsPos [l, x, adjoinsPos e]]
        _ -> before ++ (l:after)

rewriteLet :: [Tree] -> [Tree]
rewriteLet = tripBy (`kwIs` Let) (id, go)
    where
    go before l after@[QBranch _ body] = tripBy (`kwIs` In) (const invalidLetIn, rewriteIn) body
        where
        invalidLetIn = before ++ [l `adjoinPos` adjoinsPos after]
        rewriteIn []   i []   = before ++ [l,               i]
        rewriteIn defs i []   = before ++ [l, inBlock defs, i]
        rewriteIn []   i body = before ++ [l,               i, inBlock body]
        rewriteIn defs i body = before ++ [l, inBlock defs, i, inBlock body]
        inBlock [] = error "Murex.Sugar.Desugar.inBlock: can't construct node from nothing"
        inBlock [x] = x
        inBlock xs@(x:_) = QLeaf (getPos x) (Kw Block) `adjoinslPos` xs
    go before l [def, i, body] | i `kwIs` In = before ++ [l, def, i, body]
    go before l after = before ++ [l `adjoinPos` adjoinsPos after]


------ Helpers ------
kwIs :: Tree -> Keyword -> Bool
kwIs (QLeaf _ (Kw kw')) kw | kw == kw' = True
kwIs _ _ = False

kwElem :: Tree -> [Keyword] -> Bool
kwElem (QLeaf _ (Kw kw')) kws | kw' `elem` kws = True
kwElem _ _ = False

isName (QLeaf _ (Name _)) = True
isName _ = False

isLabel (QLeaf _ (Label _)) = True
isLabel _ = False

isNode (QBranch _ _) = True
isNode _ = False

