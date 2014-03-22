module Murex.Parser (
      Atom(..)
    , Primitive(..)
    , runParser
    ) where

import Import
import Murex.Syntax.Concrete
import qualified Murex.Syntax.Abstract as A
import Murex.Lexer (Token)
import qualified Murex.Lexer as Lex
import Data.Hierarchy
import Data.Hexpr

import qualified Text.Parsec as P
import Text.Parsec ( Parsec, SourcePos, ParseError, tokenPrim, getPosition, (<?>)
                   , between, many1, sepBy, sepEndBy, sepBy1
                   , choice, parserZero, eof)


type Parser = Parsec [Pos Token] ()

runParser :: [Pos Token] -> Either ParseError [QTree]
runParser input = P.runParser murex () "" input

murex :: Parser [QTree]
murex = do
    P.getInput >>= \input -> case input of { [] -> return (); (x:xs) -> P.setPosition (fst x) }
    res <- bareNode `sepBy` token Lex.Newline
    eof
    return res


------ Core Combinators ------
fullyBareNode :: Parser [QTree]
fullyBareNode = many1 (atom <|> expression)
bareNode = adjoinsPos <$> fullyBareNode

atom :: Parser (QTree)
atom = (<?> "atom") $ choice [ longId
                             , leaf Bind bindToken
                             , leaf Label labelToken
                             , leaf Lit literalToken
                             , punctuation
                             ]
    where
    longId = leaf Name $ nameToken `sepBy1` token Lex.Dot
    punctuation = choice [ leaf (const $ Kw At) (token Lex.At)
                         , leaf (const $ Kw Ellipsis) (token Lex.Ellipsis)
                         ]

expression :: Parser (QTree)
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
        dot <- leaf (const $ Kw InfixDot) (token Lex.Dot)
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

bindToken :: Parser String
bindToken = do
    (_, Lex.Bind res) <- satisfy $ \t -> case t of
        Lex.Bind name -> True
        _ -> False
    return res

labelToken :: Parser Label
labelToken = do
    (_, Lex.Label res) <- satisfy $ \t -> case t of
        Lex.Label ix -> True
        _ -> False
    return res

literalToken :: Parser A.Literal
literalToken = do
    (_, Lex.Lit res) <- satisfy $ \t -> case t of
        Lex.Lit ix -> True
        _ -> False
    return res

satisfy :: (Token -> Bool) -> Parser (Pos Token)
satisfy p = tokenPrim show updatePos testToken
    where
    updatePos oldPos _ [] = oldPos --FIXME report the actual end of file
    updatePos _ _ (x:_) = fst x
    testToken t = if p (snd t) then Just t else Nothing


------ Helpers ------
leaf :: (a -> Atom) -> Parser a -> Parser (QTree)
leaf f p = individual <$> getPosition <*> (f <$> p)




