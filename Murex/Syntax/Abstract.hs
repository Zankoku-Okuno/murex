module Murex.Syntax.Abstract where

import Import
import Data.Sequence ( Seq, (|>), (<|), (><)
                     , ViewL(..), ViewR(..), viewl, viewr)
import qualified Data.Sequence as S
import Data.Foldable (toList)


data AST = Lit Literal
         | Sequence [AST]
         | Record (AList Label AST)
         | Variant Label AST
         | Var Symbol
         | Define AST AST
         | Lambda [Symbol] AST
         | Apply [AST]
         | Block [AST]
         | LetIn AST AST
         | Builtin Builtin
         | Project Label
         | Modify Label AST
         | Update Label AST

data Literal = MurexUnit
             | MurexBool Bool
             | MurexNum Rational
             | MurexInf | MurexNegZ | MurexNegInf | MurexNaN
--             | MurexNat Natural --package: natural-numbers
--             | MurexInt Integer
--             | MurexRat Rational
--             | MurexI64 Int64 | MurexI32 Int32 | MurexI16 Int16 | MurexI8 Int8
--             | MurexW64 Word64 | MurexW32 Word32 | MurexW16 Word16 | MurexW8 Word8
--             | MurexF64 Double | Murex32 Float
             | MurexChar Char
             | MurexSeq (Seq Literal)
             | MurexRecord (AList Label Literal)
             | MurexVariant Label Literal
--             | MurexBits TODO
--             | MurexRef (MVar Literal) | MurexSharedRef (IORef Literal)
--             | MurexArray (IOArray Int Literal)
--             | MurexChan Chan
--             | MurexHandle Handle
--             | MurexHandler TODO
    deriving (Eq)

data Builtin = PutChr | GetChr
             | PutStr | GetStr
             --TODO more general IO
             -- logic
             | NotBool | EqBool | AndBool | OrBool | XorBool
             | ElimBool
             -- arithmentic
             | NegNum | AddNum | SubNum | MulNum | QuoNum | RemNum | QuoremNum | DivNum | IPowNum | IRootNum
             -- relationals and predicates
             | EqNum | NeqNum | LtNum | GtNum | LteNum | GteNum
             | IsInfNum | IsNanNum | IsZNum | IsPosNum | IsNegNum
             -- floating point
             | Exp | Log | Pow | Root | FMA
             | Sin | Cos | Sincos | Tan | Sinh | Cosh | Sincosh | Tanh
             | ASin | ACos | ASincos | ATan | ASinh | ACosh | ASincosh | ATanh
             -- TODO floating point exceptions
             -- characters
             | EqChr
             -- conversions
             | Numer | Denom
             | NumChr | ChrNum
             --TODO more conversions
             -- sequences
             | LenSeq | IxSeq | SetSeq
             | ConsSeq | SnocSeq | CatSeq
             | HeadSeq | TailSeq | InitSeq | LastSeq
    deriving (Show, Eq)


------ Murex <-> Haskell ------
instance Show AST where
    show (Lit x) = show x
    show (Sequence xs) = "Sequence [" ++ intercalate ", " (show <$> xs) ++ "]"
    show (Record xs) = "Record [" ++ intercalate ", " (showRecordItem <$> xs) ++ "]"
    show (Variant l x) = "Variant (" ++ showRecordItem (l, x) ++ ")"
    show (Var x) = unintern x
    show (Define x e) = "Define (" ++ show x ++ ") (" ++ show e ++ ")"
    show (Lambda xs e) = "Lambda [" ++ intercalate ", " (unintern <$> xs) ++ "] (" ++ show e ++ ")"
    show (Apply es) = "Apply [" ++ intercalate ", " (show <$> es) ++ "]"
    show (Block es) = "Block [" ++ intercalate ", " (show <$> es) ++ "]"
    show (LetIn def body) = "Let (" ++ show def ++ ") (" ++ show body ++ ")"
    show (Project l) = "Project " ++ showLabel l
    show (Modify l f) = "Modify " ++ showLabel l ++ " (" ++ show f ++ ")"
    show (Update l f) = "Update " ++ showLabel l ++ " (" ++ show f ++ ")"
instance Show Literal where
    show MurexUnit = "()"
    show (MurexBool True) = "True"
    show (MurexBool False) = "False"
    show (MurexNum x) = if denominator x == 1
                            then show (numerator x)
                            else show (numerator x) ++ "/" ++ show (denominator x)
    show (MurexChar c) = show c --TODO show like murex parses
    show (MurexSeq s) = maybe (show $ toList s) show (fromMurexString s)
    show (MurexRecord xs) = "{" ++ intercalate ", " (map showRecordItem xs) ++ "}"
    show (MurexVariant l x) = "{" ++ showRecordItem (l, x) ++ "}"
showRecordItem (Left l, x) = "`" ++ show l ++ " " ++ show x
showRecordItem (Right l, x) = "`" ++ l ++ " " ++ show x
showLabel (Left l) = show l
showLabel (Right l) = l

fromMurexChar :: Literal -> Char
fromMurexChar (MurexChar c) = c
toMurexChar :: Char -> Literal
toMurexChar c = MurexChar c

toMurexString :: String -> Literal
toMurexString = MurexSeq . S.fromList . map MurexChar
fromMurexString :: Seq Literal -> Maybe String
fromMurexString s = let s' = toList s in if all isChar s'
                                    then Just (map fromMurexChar s')
                                    else Nothing
    where
    isChar (MurexChar _) = True
    isChar _ = False

