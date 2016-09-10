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
                    | BPGTE BooleanPrimary Predicate
                    | BPGT BooleanPrimary Predicate
                    | BPLTE BooleanPrimary Predicate
                    | BPLT BooleanPrimary Predicate
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

data CreateDefinition = ColumnDef { name         :: String,
                                    definition   :: ColumnDefinition,
                                    colDefRefDef :: Maybe RefDefinition
                                  }
                      | PKDef { pkColNames :: [String] }
                      | KeyDef { keyColNames :: [String] }
                      | UKDef { ukColNames :: [String] }
                      | FKDef { fkColNames :: [String],
                                fkRefDef   :: RefDefinition
                              }
                      | CheckExpr { checkExpr :: Expr }
                      deriving (Eq, Show)

data RefDefinition = RefDefinition { refDefTblName  :: String,
                                     refDefColNames :: [String]
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


-- Select Statements
--
data TableReferences = TableReferences { tableReferences :: [TableReference] }
                     deriving (Eq, Show)

data TableReference = TableReference { tableFactor :: TableFactor
                                     , joinTables  :: [JoinTable]
                                     }
                    deriving (Eq, Show)

data TableFactor = TableFactor { tableFactorName :: String }
                 | TableFactors { tableFactors :: TableReferences }
                 deriving (Eq, Show)

data JoinTable = InnerJoin { innerTableFactor :: TableFactor
                           , innerJoinConds   :: Maybe JoinCondition
                           }
               | StraightJoin { straightTableFactor :: TableFactor
                              }
               | OuterJoin { outerLeft         :: Bool
                           , outerJoinTableRef :: TableReference
                           , outerJoinCond     :: JoinCondition
                           }
               deriving (Eq, Show)

data JoinCondition = JoinExpr { joinExpr :: Expr }
                   -- | JoinUsing { joinColNames :: [String] }
                   deriving (Eq, Show)

data SelectStmt = Select { selectAll       :: Bool
                         , selectDistinct  :: Bool
                         , selectExprs     :: [Expr]
                         , selectTabRefs   :: Maybe TableReferences
                         , selectWhereCond :: Maybe Expr
                         }
                  deriving (Eq, Show)
