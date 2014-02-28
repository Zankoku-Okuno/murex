module Murex.Sugar where

import Import hiding (hoistEither)
import Data.Hierarchy
import Data.Hexpr
import Control.Monad.Errors
import Text.Parsec (ParsecT, ParseError, tokenPrim)
import qualified Text.Parsec as P
import Murex.Syntax.Concrete


type Desugar = ParsecT [Tree] () (Errors [ParseError])

runDesugar :: Desugar Tree -> Tree -> Either [ParseError] Tree
runDesugar parser input = case runErrors $ P.runPT parser () "" [input] of
        Left errs -> Left errs
        Right e_vals -> case e_vals of
            Left e -> Left [e]
            Right val -> Right val



word :: String -> Desugar Tree
word name = token checkWord
    where
    checkWord node@(QLeaf _ (Name [name'])) | name == name' = Just node
    checkWord _ = Nothing

leaf :: Atom -> Desugar Tree
leaf x = token checkLeaf
    where
    checkLeaf node@(QLeaf _ y) | x == y = Just node
    checkLeaf _ = Nothing

anyNode :: Desugar Tree
anyNode = token Just

shortId :: Desugar Tree
shortId = satisfy check
    where
    check (QLeaf _ (Name [_])) = True
    check _ = False


satisfy :: (Tree -> Bool) -> Desugar Tree
satisfy p = token (\x -> if p x then Just x else Nothing)

subNode :: Desugar [Tree] -> Desugar Tree
subNode parser = run =<< anyNode
    where
    run :: Tree -> Desugar Tree
    run input = case runErrors $ P.runPT wrappedParser () "" (wrap input) of
        Left errs -> lift (recover input $ err errs)
        Right e_vals -> case e_vals of
            Left e -> lift (recover input $ err1 e)
            Right vals -> return $ adjoins (getPos input) vals
    wrappedParser = do
        P.getInput >>= \input -> case input of
            [] -> return ()
            (x:xs) -> P.setPosition (getPos x)
        res <- parser
        P.getInput >>= \remaining -> case remaining of
            [] -> return res
            _ -> fail "expected end of node"
    wrap input = case input of
        QBranch _ xs -> xs
        x -> [x]

token :: (Tree -> Maybe a) -> Desugar a
token = tokenPrim show updatePos
    where
    updatePos oldPos _ [] = oldPos
    updatePos _ _ (x:_) = getPos x




