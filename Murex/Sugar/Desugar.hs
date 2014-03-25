{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Murex.Sugar.Desugar where

import Import
import Data.Hierarchy
import Data.Hexpr
import Language.Desugar
import Murex.Syntax.Concrete
import Murex.Sugar.Notation
import Language.Distfix
import Control.Monad.Errors
import Text.Parsec (ParseError)
import qualified Text.Parsec as P

detectKeywords :: [NotationConfig] -> QTree -> QTree
detectKeywords config = preorder (go keywords, id)
    where
    keywords = defaultKeywords --STUB
    go keywords (Name [x]) = case x `lookup` keywords of
        Nothing -> Name [x]
        Just kw -> Kw kw
    go keywords x = x



cannonize :: QTree -> QTree
cannonize input = let atSymbols = preorder (id, rewriteAtSign) input
                      defSorts = preorder (id, rewriteDefSorts) atSymbols
                      lambdaesque = preorder (id, rewriteLambdaesque) defSorts
                      letIn = preorder (id, rewriteLet) lambdaesque
                      infixes = foldr1 (.) (preorder <$> lowInfixes) $ letIn
                  in infixes
    where
    lowInfixes = [ reverseInfix (`kwIs` To)
                 , reverseInfix (`kwIs` Ann)
                 , forwardInfix (`kwIs` Def)
                 ]


dedistfix :: DistfixTable QTree -> QTree -> Either (DistfixError QTree) QTree
dedistfix = runDistfix

instance DistfixStructure QTree where
    unwrap (QLeaf _ _) = Nothing
    unwrap (QBranch pos xs) = Just (xs, adjoins pos)
    unwrap (Quasiquote pos x) = do
        (val, inner) <- unwrap x
        return (val, Quasiquote pos . inner)
    unwrap (Unquote pos x) = do
        (val, inner) <- unwrap x
        return (val, Unquote pos . inner)
    unwrap (Splice pos x) = do
        (val, inner) <- unwrap x
        return (val, Splice pos . inner)
    defaultRewrap = adjoinsPos
    nodeMatch (QLeaf _ x) (QLeaf _ y) = x == y
    nodeMatch _ _ = False

dotExprs :: QTree -> Either ParseError QTree
dotExprs x@(QLeaf _ _) = Right x
dotExprs (QBranch pos xs) = adjoins pos <$> (rewriteDot =<< mapM dotExprs xs)
dotExprs (Quote pos x) = Quote pos <$> dotExprs x
dotExprs (Unquote pos x) = Unquote pos <$> dotExprs x
dotExprs (Splice pos x) = Splice pos <$> dotExprs x


------ Keywords ------
defaultKeywords :: AList String Keyword
defaultKeywords = [ ("λ",      Lambda)
                  , ("Λ",      BigLambda)
                  , ("∀",      Forall)
                  , ("block",  Block)
                  , ("let",    Let)
                  , ("in",     In)
                  , ("≡",      Def)
                  , ("→",      To)
                  , (":",      Ann)
                  , ("type",   Type)
                  , ("api",    Api)
                  , ("module", Module)
                  , ("syn",    Synonym)
                  ]

------ Cannonize ------
rewriteAtSign :: [QTree] -> [QTree]
rewriteAtSign = tripBy (`kwIs` At) (id, go)
    where
    go before at after = before ++ case after of
        (QBranch _ (x:xs):after') | isLabel x -> (:after') $ case xs of
            (QLeaf _ (Kw Def):body) -> (individual (getPos at) (Prim Update) `adjoinPos` x) `adjoinPos` adjoinsPos body
            body -> (individual (getPos at) (Prim Modify) `adjoinPos` x) `adjoinPos` adjoinsPos body
        (x:xs) | isName x -> error "STUB: alias pattern"
               | isLabel x -> (QLeaf (getPos at) (Prim Project) `adjoinPos` x) : xs
        _ -> at:after
    unLabel (Label name) = name

rewriteDefSorts :: [QTree] -> [QTree]
rewriteDefSorts (x:body) | x `kwElem` sorts = [x, adjoinsPos body]
    where sorts = [Type, Api, Module, Synonym]
rewriteDefSorts other = other

rewriteLambdaesque :: [QTree] -> [QTree]
rewriteLambdaesque = tripBy isLambdaesque (id, go)
    where
    isLambdaesque = (`kwElem` [Lambda]) --TODO big lambda, forall
    go before l after = case after of
        (x:e@(_:_)) -> before ++ [adjoinsPos [l, x, adjoinsPos e]]
        _ -> before ++ (l:after)

rewriteLet :: [QTree] -> [QTree]
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

rewriteDot :: [QTree] -> Either ParseError [QTree]
rewriteDot xs = tripBy isDotExpr (Right, rewrite) xs
    where
    isDotExpr (QBranch _ [QLeaf _ (Kw InfixDot), expr]) = True
    isDotExpr _ = False
    rewrite [] dot _ = error $ "TODO Error: " ++ intercalate " " (map show xs)
    rewrite before dot after = rewriteDot (undot dot : adjoinsPos before : after)
    undot (QBranch _ [QLeaf _ (Kw InfixDot), expr]) = expr


------ Helpers ------
kwIs :: QTree -> Keyword -> Bool
kwIs (QLeaf _ (Kw kw')) kw | kw == kw' = True
kwIs _ _ = False

kwElem :: QTree -> [Keyword] -> Bool
kwElem (QLeaf _ (Kw kw')) kws | kw' `elem` kws = True
kwElem _ _ = False

isName (QLeaf _ (Name _)) = True
isName _ = False

isLabel (QLeaf _ (Label _)) = True
isLabel _ = False

