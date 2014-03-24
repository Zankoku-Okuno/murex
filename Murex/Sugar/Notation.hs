module Murex.Sugar.Notation where

import Import
import Data.Hierarchy
import Data.Hexpr
import Language.Distfix
import Control.Monad.Errors
import qualified Text.Parsec as P
import Text.Parsec (ParseError, SourceName)
import Text.Parsec.Pos (newPos)
import Murex.Syntax.Concrete
import Murex.Syntax.Abstract (Literal(MurexUnit))
import Murex.Parser

data NotationConfig = Use    SourcePos String
                    | Define SourcePos String [DistfixLevel]
                    -- TODO maybe ability to configure keywords
data DistfixLevel = AnonLevel  SourcePos [Distfix QTree]
                  | NamedLevel SourcePos String


interpretNotation :: [NotationConfig] -> Errors [ParseError] ([NotationConfig], DistfixTable QTree)
interpretNotation input = case filter isUse input of
        [] -> return (defs, [])
        [Use _ name] -> (,) defs . reverse <$> expand name
        _ -> error "multiple uses"
    where
    defs = filter isDefinition input
    isDefinition x = case x of { Define _ _ _ -> True; _ -> False }
    isUse x = case x of { Use _ _ -> True; _ -> False }
    expand name = do
        distfixes <- lookupDef name
        concat <$> mapM go distfixes
        where
        go (AnonLevel _ xs) = return [xs]
        go (NamedLevel _ name) = expand name
    lookupDef name = case filter isDefNamed defs of
        [] -> error $ "undefined notation " ++ show name
        [Define _ _ body] -> return body
        xs -> error $ "multiple notation definitions for " ++ show name
        where
        isDefNamed (Define _ name' _) | name == name' = True
        isDefNamed _ = False
extractNotation :: SourceName -> [QTree] -> Errors [ParseError] ([NotationConfig], QTree)
extractNotation sourcename input = let (directives, body) = partition isNotationDirective input
                                   in (,) <$> transNotation directives <*> pure (transBody body)
    where
    isNotationDirective (QLeaf _ (Name ["notation"])) = True
    isNotationDirective (QBranch _ (QLeaf _ (Name ["notation"]):_)) = True
    isNotationDirective _ = False
    transBody [] = individual (newPos sourcename 0 0) (Lit MurexUnit)
    transBody [body] = body
    transBody body = individual (newPos sourcename 0 0) (Kw Block) `adjoinslPos` body
    transNotation directives = catMaybes <$> mapRecover (map parseNotation directives)


parseNotation :: QTree -> Errors [ParseError] NotationConfig
--(notation use name)
parseNotation (QBranch pos (_:QLeaf _ (Name ["use"]):body)) = case body of
    [] -> error "expecting short id"
    [QLeaf nPos (Name [name])] -> Use pos <$> checkNotationName (nPos, name)
    other -> error $ "expecting short id, got: " ++ show other
parseNotation (QBranch pos [_, name, body]) = 
    case name of
        QLeaf _ (Name [name]) -> case body of
            QLeaf nPos (Name [level])
                --(notation name name)
                | '_' `notElem` level -> return $ Define pos name [NamedLevel nPos level]
                --(notation name parts)
                | otherwise           -> Define pos name . (:[]) <$> parseLevel body
            --(notation name (level (lines...)))
            QBranch _ [(QLeaf _ (Name ["level"])), _] ->
                Define pos name . (:[]) <$> parseLevel body
            --(notation name (parts assoc))
            QBranch _ [_, QLeaf _ (Name [assoc])] | assoc `elem` ["left", "right"] ->
                Define pos name . (:[]) <$> parseLevel body
            --(notation name (levels...))
            QBranch _ body ->
                Define pos name . catMaybes <$> mapRecover (map parseLevel body)
        other -> error $ "TODO expecting short id, got " ++ show other
parseNotation _ = err (error "TODO notation error reporting")

parseLevel :: QTree -> Errors [ParseError] DistfixLevel
--name
--parts
parseLevel line@(QLeaf pos (Name [name]))
    | '_' `notElem` name = NamedLevel pos         <$> checkNotationName (pos, name)
    | otherwise          = AnonLevel  pos . (:[]) <$> parseDistfixLine line
parseLevel (QLeaf pos x) = error $ "TODO expecting short id, got " ++ show x
--(level (lines...))
parseLevel (QBranch pos [QLeaf _ (Name ["level"]), body]) =
    AnonLevel pos <$> case body of
        --(level parts)
        QLeaf _ _ -> (:[]) <$> parseDistfixLine body
        --(level (parts assoc))
        QBranch _ [_, (QLeaf _ (Name [assoc]))] | assoc == "left" || assoc == "right" ->
            (:[]) <$> parseDistfixLine body
        --(level (lines...))
        QBranch _ body -> catMaybes <$> mapRecover (map parseDistfixLine body)
--(parts assoc)
parseLevel x@(QBranch pos _) = AnonLevel pos . (:[]) <$> parseDistfixLine x

parseDistfixLine :: QTree -> Errors [ParseError] (Distfix QTree)
parseDistfixLine (QLeaf pos (Name [name])) = 
    if '_' `elem` name
        then parseDistfix name (pos, Nothing)
        else error $ "expected slots (`_') in distfix definition, got: `" ++ name ++ "'"
parseDistfixLine (QBranch _ [QLeaf _ (Name [name]), assoc]) = 
    case assoc of
        QLeaf pos (Name ["left"]) -> parseDistfix name (pos, Just False)
        QLeaf pos (Name ["right"]) -> parseDistfix name (pos, Just True)
        other -> error $ "expected association (`left' or `right'), got: " ++ show other
parseDistfixLine other = error $ "expected distfix definition, got: " ++ show other

-- Nothing: unspecified, True: right, False: left
parseDistfix :: String -> (SourcePos, Maybe Bool) -> Errors [ParseError] (Distfix QTree)
parseDistfix name (pos, assoc) = uncurry (Distfix $ rewrite name) <$> do
        let (shape_m, parts) = parseDistfixName name
        shape <- checkShape assoc shape_m
        return (shape, parts)
    where
    checkShape :: Maybe Bool -> Maybe Shape -> Errors [ParseError] Shape
    checkShape Nothing Nothing = error $ "can't decide between left- or right-associative; shape spec. required for " ++ name
    checkShape Nothing (Just shape) = return shape
    checkShape _ (Just Closed) = error $ "closed distfixes have no associativity; impossible shape spec. for " ++ name
    checkShape (Just True) Nothing = return OpenRight
    checkShape (Just False) Nothing = return OpenLeft
    checkShape (Just True) (Just HalfOpenRight) = return HalfOpenRight
    checkShape (Just False) (Just HalfOpenLeft) = return HalfOpenLeft
    checkShape (Just _) _ = error $ "l/r slot can't go with r/l assoc: impossible shape spec. for " ++ name

parseDistfixName :: String -> (Maybe Shape, [QTree])
parseDistfixName name = (expectedShape, mk undefined <$> splitBody)
    where
    lSlot = head name == '_'
    rSlot = last name == '_'
    splitBody = filter (/= "_") . groupBy (\x y -> x /= '_' && y /= '_') $ name
    expectedShape = case (lSlot, rSlot) of
        (True, True) -> Nothing
        (True, False) -> Just HalfOpenLeft
        (False, True) -> Just HalfOpenRight
        (False, False) -> Just Closed
--FIXME rule out names with no underscores

checkNotationName :: (SourcePos, String) -> Errors [ParseError] String
checkNotationName (pos, name)
    | '_' `elem` name = error $ "unexpeced slots in notation name: " ++ name
    | name `elem` ["use", "level", "left", "right"] = error $ "reserved notation name: " ++ name
    | otherwise = return name


rewrite name = flip mk name . getPos . head
mk pos name = QLeaf pos (Name [name])


instance Show NotationConfig where
    show (Use _ name) = "Use " ++ name
    show (Define _ name levels) = "Define " ++ name ++ " " ++ show levels
instance Show DistfixLevel where
    show (AnonLevel _ distfixes) = "AnonLevel " ++ show distfixes
    show (NamedLevel _ name) = "NamedLevel " ++ name