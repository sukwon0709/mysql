module Syntax where

data Expr = EOr Expr Expr
          | EXOr Expr Expr
          | EAnd Expr Expr
          | ENot Expr
          | BIs BooleanPrimary Bool
          | BIsNot BooleanPrimary Bool
          | BooleanPrimary BooleanPrimary
          deriving (Eq, Show)

-- data BooleanPrimary = BPNotEq BooleanPrimary Predicate
--                     | BPComp CompOp BooleanPrimary Predicate
--                     | Predicate Predicate
--                     deriving (Eq, Show)

-- Bumped up Predicate term into BooleanPrimary to use chainl1
data BooleanPrimary = BPSafeNotEq BooleanPrimary BooleanPrimary
                    | BPEq BooleanPrimary BooleanPrimary
                    | BPGte BooleanPrimary BooleanPrimary
                    | BPGt BooleanPrimary BooleanPrimary
                    | BPLte BooleanPrimary BooleanPrimary
                    | BPLt BooleanPrimary BooleanPrimary
                    | BPNotEq BooleanPrimary BooleanPrimary
                    | Predicate Predicate
                    deriving (Eq, Show)

data Predicate = BitExpr BitExpr
               deriving (Eq, Show)

data BitExpr = BitOr BitExpr BitExpr
             | BitAnd BitExpr BitExpr
             | BitLShift BitExpr BitExpr
             | BitRShift BitExpr BitExpr
             | BitAdd BitExpr BitExpr
             | BitSub BitExpr BitExpr
             | BitMul BitExpr BitExpr
             | BitDiv BitExpr BitExpr
             | BitIntDiv BitExpr BitExpr
             | BitMod BitExpr BitExpr
             | BitXOr BitExpr BitExpr
             | SimpleExpr SimpleExpr
             deriving (Eq, Show)

data SimpleExpr = Lit Literal
                | Ident String
                | SEOr SimpleExpr SimpleExpr
                | SEPlus SimpleExpr
                | SEMinus SimpleExpr
                | SETilde SimpleExpr
                | SENot SimpleExpr
                | SEList [SimpleExpr]
                deriving (Eq, Show)

data Literal = BLit Bool
             | NLit String
             | SLit String
             | NullLiteral
             deriving (Eq, Show)
