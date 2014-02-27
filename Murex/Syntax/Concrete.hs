{-# LANGUAGE FlexibleInstances #-}
module Murex.Syntax.Concrete where

import Import
import Data.Hierarchy
import Data.Hexpr
import Murex.Data
import qualified Murex.Syntax.Typeless as A
import Data.Symbol


data Atom = Literal MurexData
          | Name [String]
          | Label (Either Integer String)
          | Kw Keyword
          | Prim Primitive
    deriving (Eq)

data Keyword = Lambda
	deriving (Eq)
data Primitive = List | Nil
               | Xons | Xil
               | At | Ellipsis
               | InfixDot
               | Interpolate
    deriving (Eq)


--FIXME DELME
toAST :: Quasihexpr a Atom -> A.AST
toAST (QLeaf _ (Literal x)) = A.Literal x
toAST (QLeaf _ (Name [name])) = A.Var (intern name)
toAST (QBranch p (QLeaf _ (Kw Lambda) : QLeaf _ (Name [name]) : body)) = A.Lambda [intern name] (toAST (adjoins p body))
toAST (QBranch p (QLeaf _ (Kw Lambda) : QBranch _ names : body)) = A.Lambda (transName <$> names) (toAST (adjoins p body))
	where
	transName (QLeaf _ (Name [name])) = intern name
toAST (QBranch _ xs) = A.Apply (toAST <$> xs)


instance Show Atom where
    show (Literal x) = show x
    show (Name parts) = intercalate "." parts
    show (Label (Left i)) = '`':show i
    show (Label (Right name)) = '`':name
    show (Kw x) = '#':show x
    show (Prim x) = show x
instance Show Primitive where
    show List = "#list"
    show Nil = "#nil"
    show Xons = "#xons"
    show Xil = "#xil"
    show At = "@"
    show Ellipsis = ".."
    show Interpolate = "#str"
instance Show Keyword where
	show Lambda = "\955"

instance Show (Hexpr SourcePos Atom) where
    show (Leaf _ x) = show x
    show (Branch _ [Leaf _ (Prim InfixDot), expr]) = '.':show expr
    show (Branch _ xs) = "(" ++ intercalate " " (map show xs) ++ ")"

instance Show (Quasihexpr SourcePos Atom) where
    show (QLeaf _ x) = show x
    show (QBranch _ [QLeaf _ (Prim InfixDot), expr]) = '.':show expr
    show (QBranch _ xs) = "(" ++ intercalate " " (map show xs) ++ ")"
    --TODO show quotation
