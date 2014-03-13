module Murex.Parser (
      Atom(..)
    , Primitive(..)
    , runParser
    ) where

import Import
import Murex.Data
import Murex.Syntax.Concrete
import Murex.Lexer (Token)
import qualified Murex.Lexer as Lex
import Data.Hierarchy
import Data.Hexpr

import qualified Text.Parsec as P
import Text.Parsec ( Parsec, SourcePos, ParseError, tokenPrim, getPosition, (<?>)
                   , between, many1, sepBy, sepEndBy, sepBy1
                   , choice, parserZero, eof)


type Parser = Parsec [Pos Token] ()

runParser :: [Pos Token] -> Either ParseError [Tree]
runParser input = P.runParser murex () "" input

murex :: Parser [Tree]
murex = do
    P.getInput >>= \input -> case input of { [] -> return (); (x:xs) -> P.setPosition (fst x) }
    res <- bareNode `sepBy` token Lex.Newline
    eof
    return res


------ Core Combinators ------
fullyBareNode :: Parser [Tree]
fullyBareNode = many1 (atom <|> expression)
bareNode = adjoinsPos <$> fullyBareNode

atom :: Parser (Tree)
atom = (<?> "atom") $ choice [ longId
                             , leaf Label labelToken
                             , leaf Literal literalToken
                             , punctuation
                             ]
    where
    longId = leaf Name $ nameToken `sepBy1` token Lex.Dot
    punctuation = choice [ leaf (const $ Prim At) (token Lex.At)
                         , leaf (const $ Prim QMark) (token Lex.QMark)
                         , leaf (const $ Prim Ellipsis) (token Lex.Ellipsis)
                         ]

expression :: Parser (Tree)
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
        token Lex.OpenParen
        res0 <- adjoins pos0 <$> fullyBareNode
        rest <- P.option [] $ do
            P.lookAhead (token Lex.Comma)
            res <- many1 commaExpr
            optional (token Lex.Comma)
            return res
        token Lex.CloseParen
        return $ case rest of
            [] -> res0
            _ -> individual pos0 (Prim Tuple) `adjoinslPos` (res0:rest)
        where
        commaExpr = do
            pos0 <- getPosition
            token Lex.Comma
            adjoins pos0 <$> fullyBareNode

    indent = adjoinsPos <$> between (token Lex.Indent) (token Lex.Dedent) (bareNode `sepBy1` token Lex.Newline)
    list = do
        pos0 <- getPosition
        token Lex.OpenBrack
        res <- bareNode `sepEndBy` token Lex.Comma
        token Lex.CloseBrack
        let list = individual pos0 (Prim List)
            nil  = individual pos0 (Prim Nil)
        return $ list `adjoinslPos` (nil:res)
    xons = do
        pos0 <- getPosition
        token Lex.OpenBrace
        res <- bareNode `sepEndBy` token Lex.Comma
        token Lex.CloseBrace
        let xons = individual pos0 (Prim Xons)
            xil = individual pos0 (Prim Xil)
        return $ xons `adjoinslPos` (xil:res)
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
leaf :: (a -> Atom) -> Parser a -> Parser (Tree)
leaf f p = individual <$> getPosition <*> (f <$> p)




