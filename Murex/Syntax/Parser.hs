{-# LANGUAGE FlexibleInstances #-}
module Murex.Syntax.Parser (
      Atom(..)
    , Primitive(..)
    , runParser
    ) where

import Import
import Murex.Data
import Murex.Syntax.Lexer (Token)
import qualified Murex.Syntax.Lexer as Lex
import Data.Hierarchy
import Data.Hexpr

import qualified Text.Parsec as P
import Text.Parsec ( Parsec, SourcePos, ParseError, tokenPrim, getPosition, (<?>)
                   , between, many1, sepBy, sepEndBy, sepBy1
                   , choice, parserZero, eof)

type Parser = Parsec [Pos Token] ()

data Atom = Literal MurexData
          | Name [String]
          | Label (Either Integer String)
          | Prim Primitive
    deriving (Eq)
          
data Primitive = List | Nil
               | Xons | Xil
               | At | Ellipsis
               | InfixDot
               | Interpolate
    deriving (Eq)

runParser :: [Pos Token] -> Either ParseError [Quasihexpr SourcePos Atom]
runParser input = P.runParser murex () "" input

murex :: Parser [Quasihexpr SourcePos Atom]
murex = do
    P.getInput >>= \input -> case input of { [] -> return (); (x:xs) -> P.setPosition (fst x) }
    res <- bareNode `sepBy` token Lex.Newline
    eof
    return res


------ Core Combinators ------
fullyBareNode :: Parser [Quasihexpr SourcePos Atom]
fullyBareNode = many1 (atom <|> expression)
bareNode = adjoinsPos <$> fullyBareNode

atom :: Parser (Quasihexpr SourcePos Atom)
atom = (<?> "atom") $ choice [ longId
                             , leaf Label labelToken
                             , leaf Literal literalToken
                             , punctuation
                             ]
    where
    longId = leaf Name $ nameToken `sepBy1` token Lex.Dot
    punctuation = choice [ leaf (const $ Prim At) (token Lex.At)
                         , leaf (const $ Prim Ellipsis) (token Lex.Ellipsis)
                         ]

expression :: Parser (Quasihexpr SourcePos Atom)
expression = choice [ parens
                    , indent
                    , list
                    , xons
                    --TODO string interp
                    , infixDot
                    ]
    where
    parens = do
        pos0 <- getPosition
        res <- between (token Lex.OpenParen) (token Lex.CloseParen) fullyBareNode
        return $ adjoins pos0 res
    indent = adjoinsPos <$> between (token Lex.Indent) (token Lex.Dedent) (bareNode `sepBy1` token Lex.Newline)
    list = do
        pos0 <- getPosition
        token Lex.OpenBrack
        res <- bareNode `sepEndBy` token Lex.Comma
        token Lex.CloseBrack
        let list = individual pos0 (Prim List)
            nil  = individual pos0 (Prim Nil)
        return $ list `adjoinPos` (nil `adjoinslPos` res)
    xons = do
        pos0 <- getPosition
        token Lex.OpenBrace
        res <- bareNode `sepEndBy` token Lex.Comma
        token Lex.CloseBrace
        let xons = individual pos0 (Prim Xons)
            xil = individual pos0 (Prim Xil)
        return $ xons `adjoinPos` (xil `adjoinslPos` res)
    infixDot = do
        dot <- leaf (const $ Prim InfixDot) (token Lex.Dot)
        expr <- atom <|> parens
        return $ dot `adjoinPos` expr


------ Primitives ------
token :: Token -> Parser ()
token t = void $ satisfy (== t)

nameToken :: Parser String
nameToken = do
    (_, Lex.Name res) <- satisfy $ \t -> case t of
        Lex.Name name -> True
        _ -> False
    return res

labelToken :: Parser (Either Integer String)
labelToken = do
    (_, Lex.Label res) <- satisfy $ \t -> case t of
        Lex.Label ix -> True
        _ -> False
    return res

literalToken :: Parser MurexData
literalToken = do
    (_, Lex.Literal res) <- satisfy $ \t -> case t of
        Lex.Literal ix -> True
        _ -> False
    return res

satisfy :: (Token -> Bool) -> Parser (Pos Token)
satisfy p = tokenPrim show updatePos testToken
    where
    updatePos oldPos _ [] = oldPos --FIXME report the actual end of file
    updatePos _ _ (x:_) = fst x
    testToken t = if p (snd t) then Just t else Nothing


------ Helpers ------
leaf :: (a -> Atom) -> Parser a -> Parser (Quasihexpr SourcePos Atom)
leaf f p = individual <$> getPosition <*> (f <$> p)


------ Show ------
instance Show Atom where
    show (Literal x) = show x
    show (Name parts) = intercalate "." parts
    show (Label (Left i)) = '`':show i
    show (Label (Right name)) = '`':name
    show (Prim x) = show x
instance Show Primitive where
    show List = "#list"
    show Nil = "#nil"
    show Xons = "#xons"
    show Xil = "#xil"
    show At = "@"
    show Ellipsis = ".."
    show Interpolate = "#str"

instance Show (Hexpr SourcePos Atom) where
    show (Leaf _ x) = show x
    show (Branch _ [Leaf _ (Prim InfixDot), expr]) = '.':show expr
    show (Branch _ xs) = "(" ++ intercalate " " (map show xs) ++ ")"

instance Show (Quasihexpr SourcePos Atom) where
    show (QLeaf _ x) = show x
    show (QBranch _ [QLeaf _ (Prim InfixDot), expr]) = '.':show expr
    show (QBranch _ xs) = "(" ++ intercalate " " (map show xs) ++ ")"
    --TODO show quotation




