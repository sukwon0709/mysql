module Syntax where


-- Expression Syntax
--

data Expr = EOr Expr Expr
          | EXOr Expr Expr
          | EAnd Expr Expr
          | ENot Expr
          | BIs BooleanPrimary Bool
          | BIsNot BooleanPrimary Bool
          | BooleanPrimary BooleanPrimary
          deriving (Eq, Show)

data BooleanPrimary = BPSafeNotEq BooleanPrimary Predicate
                    | BPEq BooleanPrimary Predicate
                    | BPGte BooleanPrimary Predicate
                    | BPGt BooleanPrimary Predicate
                    | BPLte BooleanPrimary Predicate
                    | BPLt BooleanPrimary Predicate
                    | BPNotEq BooleanPrimary Predicate
                    | Predicate Predicate
                    deriving (Eq, Show)

data Predicate = PredInExprList BitExpr SimpleExpr
               | PredNotInExprList BitExpr SimpleExpr
               | BitExpr BitExpr
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
                | SEList [Expr]
                deriving (Eq, Show)

data Literal = BLit Bool
             | NLit String
             | SLit String
             | NullLiteral
             deriving (Eq, Show)


-- Statement Syntax
--

-- Create Table Statements
--
data CreateTableStmt = CreateTableStmt { isTemporary       :: Bool,
                                         tblName           :: String,
                                         createDefinitions :: [CreateDefinition]
                                       }
                       deriving (Eq, Show)

data CreateDefinition = ColumnDef { name       :: String,
                                    definition :: ColumnDefinition
                                  }
                      deriving (Eq, Show)

data ColumnDefinition = FieldDef { fieldType     :: DataType,
                                   nullable      :: Bool,
                                   autoIncrement :: Bool,
                                   uniqueKey     :: Bool,
                                   primaryKey    :: Bool
                                 }
                        deriving (Eq, Show)

data DataType = DTypeBit (Maybe Integer)                    -- BIT [(length)]
              | DTypeTinyInt (Maybe Integer)                -- TINYINT [(length)]
              | DTypeSmallInt (Maybe Integer)               -- SMALLINT [(length)]
              | DTypeMediumInt (Maybe Integer)              -- MEDIUMINT [(lenght)]
              | DTypeInt (Maybe Integer)                    -- INT [(length)]
              | DTypeInteger (Maybe Integer)                -- INTEGER [(length)]
              | DTypeBigInt (Maybe Integer)                 -- BIGINT [(length)]
              | DTypeChar (Maybe Integer)                   -- CHAR [(length)]
              | DTypeVarChar (Maybe Integer)                -- VARCHAR [(length)]
              deriving (Eq, Show)
