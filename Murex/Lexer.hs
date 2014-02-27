module Murex.Lexer (
      runLexer
    , Token (..)
    ) where

import Import hiding ((<|>))
import Control.Monad.Either
import Murex.Data
import qualified Data.Char as C
import Text.Parsec ( Parsec, ParseError, SourceName, runParser
                   , parserZero, anyChar, satisfy, char, eof, oneOf
                   , try, (<?>), many1, between
                   , getPosition, getState, modifyState)
import qualified Text.Parsec as P
import Text.Parsec.Error
import Language.Parse

type Lexer = Parsec String [Maybe Int]
--TODO Reader for config
--TODO Writer for unicodizing
--TODO keep in mind: later ability to colorize this stuff

data Token = Space
           | Indent | Newline | Dedent | OpenParen | CloseParen
           | OpenBrack | CloseBrack | OpenBrace | CloseBrace
           | OpenInterp String | CloseInterp String
           | Dot | Comma | Ellipsis | At
           | Quote | Quasiquote | Unquote | Splice
           | Name String | Label (Either Integer String)
           | Literal MurexData
    deriving (Eq)


runLexer :: SourceName -> String -> Either ParseError [Pos Token]
runLexer source input = runEither $ do
    raw <- hoistEither $ runParser murex [] source input
    return $ sanityCheck raw
    hoistEither $ postprocess raw
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
               , literal --must come before open paren, before name
               , opener
               , closer
               , punctuation
               , quotation
               , indentation
               , indentMark
               , label
               , name
               ]

sanityCheck :: [Pos Token] -> [Pos Token]
sanityCheck xs = (filterDoubleSpaces . filterEndspaces) xs
    where
    filterEndspaces [] = []
    filterEndspaces xs = case snd (head xs) of
        Space -> die
        Newline -> die
        Indent -> die
        Dedent -> die
        _ -> case snd (last xs) of
            Space -> die
            Newline -> die
            Indent -> die
    filterDoubleSpaces [] = xs
    filterDoubleSpaces ((_,Space):(_,Space):xs) = die
    filterDoubleSpaces (x:xs) = filterDoubleSpaces xs
    die = error $ "INTERNAL ERROR: sanity check fail\n" ++ show xs

postprocess :: [Pos Token] -> Either ParseError [Pos Token]
postprocess input = go input
    where
    go [] = Right stripSpaces
    go (x:[]) = Right stripSpaces
    go ((_,Space):xs) = go xs
    go ((posX, x):allY@(posY, y):xs) = case (needsSpacey x, isSpacey y) of
        (True, True) -> go (allY:xs)
        (True, False) -> if isException x y
                            then go (allY:xs)
                            else Left $ addErrorMessage (Expect "whitespace") $ newErrorMessage (SysUnExpect $ show y) posY
        (False, _) -> go (allY:xs)
    isSpacey x = x `elem` [Space, Indent, Newline, Dedent, CloseParen, CloseBrack, CloseBrace, Comma, Ellipsis]
    needsSpacey (Name _) = True
    needsSpacey (Label _) = True
    needsSpacey (Literal _) = True
    needsSpacey _ = False
    isException (Name _) Dot = True
    isException _ _ = False
    stripSpaces = filter ((/= Space) . snd) input


------ Core Combinators ------
whitespace :: Lexer (Pos Token)
whitespace = withPos (const Space <$> many1 space)

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

punctuation :: Lexer (Pos Token)
punctuation = withPos $ choice [ const Ellipsis <$> string ".."
                               , const Dot <$> char '.'
                               , const Comma <$> char ','
                               , const At <$> char '@'
                               ]

quotation :: Lexer (Pos Token)
-- NOTE: I'm reserving all the quine corners, in case I decide to bracket instead of prefix quotation
quotation = (<?> "quotation") $ withPos $ choice [ const Quasiquote <$> char '⌜'
                             , const Unquote <$> char '⌞'
                             , const Splice <$> char '⌟'
                             ]

name :: Lexer (Pos Token)
name = (<?> "identifier") $ withPos $ do
    base <- many2 (blacklistChar restrictedFromStartOfName) (blacklistChar restrictedFromName)
    primes <- many (char '\'')
    return $ Name (base ++ primes)

label :: Lexer (Pos Token)
label = (<?> "label") $ withPos $ do
    char '`'
    Label <$> ((Left <$> numLabel) <|> (Right <$> nameLabel))
    where
    nameLabel = many2 (blacklistChar restrictedFromStartOfName) (blacklistChar restrictedFromName)
    numLabel = stringToInteger 10 <$> many2 (oneOf "123456789") (oneOf "0123456789")

literal :: Lexer (Pos Token)
literal = withPos $ Literal <$> choice [ unitLit
                                       , numLit
                                       , charLit
                                       , strLit
                                       ]
    where
    unitLit = const MurexUnit <$> string "()"
    numLit = MurexNum <$> anyNumber
    charLit = (<?> "character") $ MurexChar <$> between2 (char '\'') literalChar
    strLit = (<?> "string") $ try $ toMurexString <$> between2 (char '\"') (catMaybes <$> many maybeLiteralChar)
    --TODO string interpolation: needs nesting stack

comment :: Lexer ()
comment = blockComment P.<|> lineComment --block must come before line
    where
    lineComment = void $ char '#' >> anyChar `manyTill` newline
    blockComment = oneBlock
        where
        oneBlock = void $ string "#{" >> inBlock `manyThru` string "}#"
        inBlock = oneBlock P.<|> void anyChar

restrictedFromName :: Char -> Bool
restrictedFromName c = c `elem` "\"\'`\\#.,@()[]{}⌜⌞⌟⌝"

restrictedFromStartOfName :: Char -> Bool
restrictedFromStartOfName c = c `elem` "0123456789" || restrictedFromName c


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

instance Show Token where
    show Space = "whitespace"
    show Indent = "indent"
    show Newline = "newline"
    show Dedent = "dedent"
    show OpenParen = "`('"
    show OpenBrack = "`['"
    show OpenBrace = "`{'"
    show CloseParen = "`)'"
    show CloseBrack = "`]'"
    show CloseBrace = "`}'"
    show Dot = "dot"
    show Comma = "comma"
    show Ellipsis = "`..'"
    show At = "`@'"
    show Quote = error "not using Quote tokens"
    show Quasiquote = "`⌜'"
    show Unquote = "`⌞'"
    show Splice = "`⌟'"
    show (Name name) = "name (" ++ name ++ ")"
    show (Label (Left i)) = "label (" ++ show i ++ ")"
    show (Label (Right name)) = "label (" ++ name ++ ")"
    show (Literal MurexUnit) = "literal (unit)"
    show (Literal x) = "literal (" ++ show x ++ ")"

------ Basic Combinators ------
space :: Lexer ()
space = (<?> "whitespace") $ do
    indentEnabled <- isJust <$> peek
    let spaceParsers' = if indentEnabled then spaceParsers else newline:spaceParsers
    choice spaceParsers'
    where
    spaceParsers = [ void $ oneOf " \t" --TODO maybe more inline whitespace
                   , void $ string "\\\n"
                   , comment
                   ]

newline :: Lexer ()
newline = (simpleNewline >> skipBlankLines) P.<|> eof

skipBlankLines :: Lexer ()
skipBlankLines = (<?> "whitespace") $ maybe (return ()) (const go) =<< peek
    where
    go = isBlankLine >>= \blank -> if blank then blankLine >> go else return ()
    isBlankLine = (lookAhead blankLine >> return True) P.<|> return False
    blankLine = (many space >> simpleNewline) <|> (many1 space >> eof)

simpleNewline :: Lexer ()
simpleNewline = void (char '\n' <?> "newline")




