module Murex.Syntax.Lexer (
      runLexer
    , Token (..)
    ) where

import Import
import Murex.Data
import Text.Parsec ( Parsec, ParseError, SourceName, runParser
                   , parserZero, anyChar, satisfy, char, eof, oneOf
                   , try, (<?>), choice, many1, between
                   , getPosition, getState, modifyState)
import qualified Text.Parsec as P

type Lexer = Parsec String [Maybe Int]
--TODO Reader for config
--TODO Writer for unicodizing

data Token = Space
           | Indent | Newline | Dedent | OpenParen | CloseParen
           | OpenBrack | CloseBrack | OpenBrace | CloseBrace
           | Quote | Quasiquote | Unquote | Splice
           | Name String | Label String
           | Literal MurexData
    deriving (Show) --TODO custom show


runLexer :: SourceName -> String -> Either ParseError [Pos Token]
runLexer source input = runParser murex [] source input
--TODO preprocess literate input (bird foot to space, blank out lines that don't start w/ birdfeet)

murex :: Lexer [Pos Token]
murex = between initialize finalize (many token)
    where
    initialize = (<?> "") $ do
        push 0 *> skipBlankLines <* pop
        P.optional (char '\n')
        push =<< length <$> many (char ' ')
    finalize = do
        stackNull >>= flip when (fail "expecting dedent") 
        skipBlankLines >> eof

token :: Lexer (Pos Token)
token = choice [ whitespace
               , opener
               , closer
               , indentation
               , indentMark
               ]


whitespace :: Lexer (Pos Token)
whitespace = withPos (const Space <$> many1 space)

opener :: Lexer (Pos Token)
opener = withPos $ choice [ const OpenParen <$> char '('
                          , const OpenBrack <$> char '['
                          , const OpenBrace <$> char '{'
                          ] <* disable
closer :: Lexer (Pos Token)
closer = withPos $ choice [ const CloseParen <$> char ')'
                          , const CloseBrack <$> char ']'
                          , const CloseBrace <$> char '}'
                          ] <* popDisable

indentation :: Lexer (Pos Token)
indentation = withPos $ do
    stackNull >>= flip when parserZero
    expect <- peek >>= maybe parserZero return
    found <- lookAhead leadingSpaces
    case compare found expect of
        GT -> leadingSpaces >> push found >> return Indent
        EQ -> leadingSpaces >> isEof >>= \end ->
              if end then parserZero else return Newline
        LT -> (<?> "dedent") $ do
            pop
            end <- isEof
            when (not end) $ do
                stackNull >>= flip when (fail "dedent does not have corresponding indent")
                expect' <- peek >>= maybe (error "indentation disable under enable") return
                when (expect' < found) $ fail "dedent does not have corresponding indent"
            return Dedent

indentMark :: Lexer (Pos Token)
indentMark = (<?> "indent") $ withPos $ do
    maybe parserZero return =<< peek
    string "\\\\"
    lookAhead $ char '\n'
    return Space

--TODO dot, comma, ellipsis (..), identifier, label, quotation, literals
--TODO string interpolation

comment :: Lexer ()
comment = blockComment <|> lineComment --block must come before line
    where
    lineComment = void $ char '#' >> anyChar `manyTill` newline
    blockComment = oneBlock
        where
        oneBlock = void $ string "#{" >> inBlock `manyThru` string "}#"
        inBlock = oneBlock <|> void anyChar


------ Helpers ------
leadingSpaces :: Lexer Int
leadingSpaces = newline >> length <$> many (char ' ')

withPos :: Lexer a -> Lexer (Pos a)
withPos lexer = (,) <$> getPosition <*> try lexer

push :: Int -> Lexer ()
push n = modifyState (Just n:)
peek :: Lexer (Maybe Int)
peek = head <$> getState
pop :: Lexer ()
pop = maybe parserZero (const $ modifyState tail) =<< peek
disable :: Lexer ()
disable = modifyState (Nothing:)
popDisable :: Lexer ()
popDisable = maybe (modifyState tail) (const parserZero) =<< peek

stackNull :: Lexer Bool
stackNull = null <$> getState


------ Basic Combinators ------
space :: Lexer ()
space = (<?> "whitespace") $ do
    indentEnabled <- isJust <$> peek
    let spaceParsers' = if indentEnabled then spaceParsers else newline:spaceParsers
    choice spaceParsers'
    where
    spaceParsers = [ void $ oneOf " \t"
                   , void $ string "\\\n"
                   , comment
                   ] --TODO more inline whitespace

newline :: Lexer ()
newline = (simpleNewline >> skipBlankLines) <|> eof

skipBlankLines :: Lexer ()
skipBlankLines = (<?> "whitespace") $ maybe (return ()) (const go) =<< peek
    where
    go = isBlankLine >>= \blank -> if blank then blankLine >> go else return ()
    isBlankLine = (lookAhead blankLine >> return True) <|> return False
    blankLine = try (many space >> simpleNewline) <|> (many1 space >> eof)

isEof :: Lexer Bool
isEof = (eof >> return True) <|> return False

string :: String -> Lexer String
string = try . P.string

lookAhead :: Lexer a -> Lexer a
lookAhead = try . P.lookAhead

manyTill :: Lexer a -> Lexer b -> Lexer [a]
manyTill p e = P.manyTill p (lookAhead e)

manyThru :: Lexer a -> Lexer b -> Lexer [a]
manyThru p e = P.manyTill p (try e)

simpleNewline = void (char '\n' <?> "newline")

