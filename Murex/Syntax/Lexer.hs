module Murex.Syntax.Lexer (
      runLexer
    , Token (..)
    ) where

import Import hiding ((<|>))
import Murex.Data
import qualified Data.Char as C
import Text.Parsec ( Parsec, ParseError, SourceName, runParser
                   , parserZero, anyChar, satisfy, char, eof, oneOf
                   , try, (<?>), many1, between
                   , getPosition, getState, modifyState)
import qualified Text.Parsec as P

type Lexer = Parsec String [Maybe Int]
--TODO Reader for config
--TODO Writer for unicodizing
--TODO keep in mind: later ability to colorize this stuff

data Token = Space
           | Indent | Newline | Dedent | OpenParen | CloseParen
           | OpenBrack | CloseBrack | OpenBrace | CloseBrace
           | Dot | Comma | Ellipsis
           | Quote | Quasiquote | Unquote | Splice
           | Name String | Label (Either Integer String)
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
               , punctuation
               , quotation
               , indentation
               , indentMark
               , label
               , literal
               , name
               ]

--TODO sanity check: no leading/trailing space, no double spaces


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
                               ]

quotation :: Lexer (Pos Token)
-- NOTE: I'm reserving all the quine corners, in case I decide to bracket instead of prefix quotation
quotation = (<?> "quotation") $ withPos $ choice [ const Quasiquote <$> char '⌜'
                             , const Unquote <$> char '⌞'
                             , const Splice <$> char '⌟'
                             ]

name :: Lexer (Pos Token)
name = (<?> "identifier") $ withPos $ do
    base <- many2 (codingChar restrictedFromStartOfName) (codingChar restrictedFromName)
    primes <- many (char '\'')
    return $ Name (base ++ primes)

label :: Lexer (Pos Token)
label = (<?> "label") $ withPos $ do
    char '`'
    Label <$> ((Left <$> numLabel) <|> (Right <$> nameLabel))
    where
    nameLabel = many2 (codingChar restrictedFromStartOfName) (codingChar restrictedFromName)
    numLabel = stringToInteger 10 <$> many2 (oneOf "123456789") (oneOf "0123456789")

literal :: Lexer (Pos Token)
literal = withPos $ Literal <$> choice [ numLit
                                       , charLit
                                       , strLit
                                       ]
    where
    numLit = (<?> "number") $ try $ do
        sign <- signLiteral
        base <- baseLiteral
        whole <- naturalLiteral base
        n <- choice [ scientificNotation whole base
                    , fractionNotation whole base
                    , return (whole % 1)
                    ]
        if n == (0 % 1) && sign == -1
            then return MurexNegZ
            else return $ MurexNum (fromIntegral sign * n)
        where
        scientificNotation whole base = do
            mantissa <- mantissaLiteral base
            exponent <- exponentLiteral
            return $ ((whole % 1) + mantissa) * exponent
        fractionNotation whole base = do
            d <- char '/' >> naturalLiteral base
            if d == 0 then parserZero else return (whole % d)

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
restrictedFromName c = c `elem` "\"\'`\\#.,()[]{}⌜⌞⌟⌝"

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


------ Fix Parsec ------
isEof = (eof >> return True) P.<|> return False

string = try . P.string

lookAhead = try . P.lookAhead

manyTill p e = P.manyTill p (lookAhead e)

manyThru p e = P.manyTill p (try e)

a <|> b = try a P.<|> b

choice = P.choice . map try

between2 e p = between e e p

many2 :: Lexer a -> Lexer a -> Lexer [a]
many2 p ps = do
    car <- p
    cdr <- many ps
    return (car:cdr)


------ Refactorable ------
-- TODO move this into hexpr
codingChar :: (Char -> Bool) -> Lexer Char
codingChar p = satisfy $ \c -> isCodingChar c && not (p c)
    where
    isCodingChar c = case C.generalCategory c of
        C.Space -> False
        C.LineSeparator -> False
        C.ParagraphSeparator -> False
        C.Control -> False
        C.Format -> False
        C.Surrogate -> False
        C.PrivateUse -> False
        C.NotAssigned -> False
        _ -> True --Letter, Mark, Number, Punctuation/Quote, Symbol

signLiteral :: Lexer Integer
signLiteral = P.option 1 $ (char '-' >> return (-1)) <|> (char '+' >> return 1)

baseLiteral :: Lexer Int
baseLiteral = choice [ (string "0x" <|> string "0X") >> return 16
                     , (string "0o" <|> string "0O") >> return  8
                     , (string "0b" <|> string "0B") >> return  2
                     ,                                  return 10
                     ]

naturalLiteral :: Int -> Lexer Integer
naturalLiteral base = stringToInteger base <$> many1 (xDigit base)

mantissaLiteral :: Int -> Lexer Rational
mantissaLiteral base = do
    char '.'
    stringToMantissa base <$> many1 (xDigit base)

--FIXME parameterized base
exponentLiteral :: Lexer Rational
exponentLiteral = P.option 1 (decimalExp <|> hexExp)
    where
    decimalExp = do
        oneOf "eE"
        sign <- signLiteral
        e <- naturalLiteral 10
        return $ 10 ^^ (sign * e)
    hexExp = do
        oneOf "hH"
        sign <- P.option 1 $ (char '-' >> return (-1)) <|> (char '+' >> return 1)
        e <- naturalLiteral 16
        return $ 16 ^^ (sign * e)

xDigit :: Int -> Lexer Char
xDigit base = case base of
    2  -> oneOf "01"
    8  -> P.octDigit
    10 -> P.digit
    16 -> P.hexDigit
    _ -> error "unrecognized base in Murex.Syntax.Lexer.xDigit"

literalChar :: Lexer Char
literalChar = normalChar P.<|> escape
    where
    normalChar = satisfy (\c -> c >= ' ' && c `notElem` "\DEL\'\"\\") --FIXME limit this slightly more
    escape = char '\\' >> choice [specialEscape, numericalEscape]
    specialEscape = fromJust . flip lookup table <$> oneOf (map fst table)
        where table = [ ('0' , '\0')
                      , ('a' , '\a')
                      , ('b' , '\b')
                      , ('e' , '\27')
                      , ('f' , '\f')
                      , ('n' , '\n')
                      , ('r' , '\r')
                      , ('t' , '\t')
                      , ('\'', '\'')
                      , ('\"', '\"')
                      , ('\\', '\\')
                      ]
    numericalEscape = chr . fromInteger <$> choice [ascii16, uni4, ascii8, uni6]
    ascii8  = stringToInteger 8  <$> (oneOf "oO" >> P.count 3 P.octDigit)
    ascii16 = stringToInteger 16 <$> (oneOf "xX" >> P.count 2 P.hexDigit)
    uni4    = stringToInteger 16 <$> (char  'u'  >> P.count 4 P.hexDigit)
    uni6    =                         char   'U' >> (high <|> low)
        where
        low  =                 stringToInteger 16 <$> (char    '0' >> P.count 5 P.hexDigit)
        high =  (+ 0x100000) . stringToInteger 16 <$> (string "10" >> P.count 4 P.hexDigit)

maybeLiteralChar :: Lexer (Maybe Char)
maybeLiteralChar = choice [ Just <$> literalChar
                          , const Nothing <$> string "\&"
                          , const Nothing <$> lineContinue
                          ]
    where
    lineContinue = between2 (char '\\') (many $ oneOf " \t\n\r")


stringToInteger :: Int -> String -> Integer
stringToInteger base = foldl impl 0
    where impl acc x = acc * fromIntegral base + (fromIntegral . digitToInt) x

stringToMantissa :: Int -> String -> Ratio Integer
stringToMantissa base = (/ (fromIntegral base%1)) . foldr impl (0 % 1)
    where impl x acc = acc / (fromIntegral base%1) + (((%1) . fromIntegral . digitToInt) x)

