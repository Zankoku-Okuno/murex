module Murex.Data where

import Import
import Data.Char
import Data.Sequence ( Seq, (|>), (<|), (><)
                     , ViewL(..), ViewR(..), viewl, viewr)
import qualified Data.Sequence as S
import Data.Foldable (toList)


type Label = Either Int String

data MurexData = MurexUnit
               | MurexBool Bool
               | MurexNum Rational
               | MurexInf | MurexNegZ | MurexNegInf | MurexNaN
--               | MurexNat Natural --package: natural-numbers
--               | MurexInt Integer
--               | MurexRat Rational
--               | MurexI64 Int64 | MurexI32 Int32 | MurexI16 Int16 | MurexI8 Int8
--               | MurexW64 Word64 | MurexW32 Word32 | MurexW16 Word16 | MurexW8 Word8
--               | MurexF64 Double | Murex32 Float
               | MurexChar Char
               | MurexSeq (Seq MurexData)
               | MurexRecord (AList Label MurexData)
               | MurexVariant Label MurexData
--               | MurexBits TODO
--               | MurexRef (MVar MurexData) | MurexSharedRef (IORef MurexData)
--               | MurexArray (IOArray Int MurexData)
--               | MurexChan Chan
--               | MurexHandle Handle
--               | MurexHandler TODO
    deriving (Eq)

------ Logic ------
notBool (MurexBool a) = MurexBool $ not a
eqBool (MurexBool a) (MurexBool b) = MurexBool $ a == b
andBool (MurexBool a) (MurexBool b) = MurexBool $ a && b
orBool (MurexBool a) (MurexBool b) = MurexBool $ a || b
xorBool (MurexBool a) (MurexBool b) = MurexBool $ a /= b


------ Arithmetic ------
negNum (MurexNum a) = MurexNum (negate a)
-- turns out, floating point arithmetic is hard. error conditions have to be put in sticky flags, which means I need access to some stateful flags
addNum :: MurexData -> MurexData -> MurexData
addNum (MurexNum a) (MurexNum b) = MurexNum (a + b)
----TODO verify these choice against floating point standards
----computations involving NaN
--addNum MurexNaN _ = MurexNaN
--addNum _ MurexNaN = MurexNaN
----computations involving infinities
--addNum MurexInf MurexNegInf = MurexNaN
--addNum MurexNegInf MurexInf = MurexNaN
--addNum MurexInf _ = MurexInf
--addNum _ MurexInf = MurexInf
--addNum MurexNegInf _ = MurexNegInf
--addNum _ MurexNegInf = MurexNegInf
----computations involving negative zero
--addNum MurexNegZ MurexNegZ = MurexNegZ
--addNum MurexNegZ (MurexNum a) = MurexNum a
--addNum (MurexNum a) MurexNegZ = MurexNum a

subNum (MurexNum a) (MurexNum b) = MurexNum (a - b)
mulNum (MurexNum a) (MurexNum b) = MurexNum (a * b)
divNum (MurexNum a) (MurexNum b) = if numerator b == 0 then Nothing else Just (MurexNum (a / b))


------ Relations ------
eqNum (MurexNum a) (MurexNum b) = MurexBool (a == b)
neqNum (MurexNum a) (MurexNum b) = MurexBool (a /= b)
ltNum (MurexNum a) (MurexNum b) = MurexBool (a < b)
lteNum (MurexNum a) (MurexNum b) = MurexBool (a > b)
gtNum (MurexNum a) (MurexNum b) = MurexBool (a <= b)
gteNum (MurexNum a) (MurexNum b) = MurexBool (a >= b)


------ Characters ------
eqChr (MurexChar a) (MurexChar b) = MurexBool (a == b)


------ Conversion ------

--numFloor (MurexNum a) = MurexInt (numerator a `div` denominator a)
--numCeil (MurexNum a) = MurexInt $ if denominator a == 1 then numerator a else (numerator a `div` denominator a) + 1

--numNumer (MurexNum a) = MurexInt $ numerator a
--numDenom (MurexNum a) = MurexInt $ denominator a

numToChr :: MurexData -> Maybe MurexData
numToChr (MurexNum a) = if valid then Just (MurexChar $ chr int) else Nothing
    where
    valid = denominator a == 1 && 0 <= int && int <= 0x10FFFF
    int = fromInteger $ numerator a
chrToNum :: MurexData -> MurexData
chrToNum (MurexChar c) = MurexNum (fromIntegral (ord c) % 1)


------ Sequences ------
lenSeq :: MurexData -> MurexData
lenSeq (MurexSeq xs) = MurexNum (fromIntegral (S.length xs) % 1)
ixSeq :: MurexData -> Int -> Maybe MurexData
ixSeq (MurexSeq xs) i = if inBounds i xs then Just (S.index xs i) else Nothing
setSeq :: MurexData -> Int -> MurexData -> Maybe MurexData
setSeq (MurexSeq xs) i x = if inBounds i xs then Just (MurexSeq $ S.update i x xs) else Nothing
inBounds i xs = 0 <= i && i < S.length xs

consSeq :: MurexData -> MurexData -> MurexData
consSeq x (MurexSeq xs) = MurexSeq $ x <| xs
snocSeq :: MurexData -> MurexData -> MurexData
snocSeq (MurexSeq xs) x = MurexSeq $ xs |> x
catSeq :: MurexData -> MurexData -> MurexData
catSeq (MurexSeq a) (MurexSeq b) = MurexSeq $ a >< b
--TODO split seq at index

headSeq (MurexSeq xs) = case viewl xs of { EmptyL -> Nothing; x :< _ -> Just x }
lastSeq (MurexSeq xs) = case viewr xs of { EmptyR -> Nothing; _ :> x -> Just x }
tailSeq (MurexSeq xs) = case viewl xs of { EmptyL -> Nothing; _ :< xs -> Just (MurexSeq xs) }
initSeq (MurexSeq xs) = case viewr xs of { EmptyR -> Nothing; xs :> _ -> Just (MurexSeq xs) }


------ Finite Types ------
project :: MurexData -> Label -> Maybe MurexData
project (MurexRecord xs) i = lookup i xs
project (MurexVariant i0 x) i = if i == i0 then Just x else Nothing

setField :: MurexData -> Label -> MurexData -> MurexData
setField (MurexRecord xs) i x = MurexRecord $ (i, x) : filter ((i /=) . fst) xs


------ Instances ------
instance Show MurexData where
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
showRecordItem (l, x) = "`" ++ show l ++ " " ++ show x


------ Builtins ------
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
fromMurexChar :: MurexData -> Char
fromMurexChar (MurexChar c) = c
toMurexChar :: Char -> MurexData
toMurexChar c = MurexChar c

toMurexString :: String -> MurexData
toMurexString = MurexSeq . S.fromList . map MurexChar
fromMurexString :: Seq MurexData -> Maybe String
fromMurexString s = let s' = toList s in if all isChar s'
                                    then Just (map fromMurexChar s')
                                    else Nothing
    where
    isChar (MurexChar _) = True
    isChar _ = False
