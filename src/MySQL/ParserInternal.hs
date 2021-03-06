{-# LANGUAGE FlexibleContexts #-}
module MySQL.ParserInternal where

import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import Text.Parsec.Pos (SourcePos)
import qualified Text.Parsec.Expr as ParExp

import qualified MySQL.Lexer                  as Lex
import qualified MySQL.Token                  as Tok
import qualified MySQL.Syntax as Syn
import Data.Functor.Identity
import Data.Char (toLower)
import Control.Applicative (liftA2)
import Control.Monad (foldM)


type Parser = Parsec [Tok.LToken] ()


-- Need to implement my version of satisfy
satisfy :: (Stream [Tok.LToken] m Tok.LToken)
        => (Tok.LToken -> Bool)
        -> ParsecT [Tok.LToken] u m Tok.LToken
satisfy f = tokenPrim show nextPos tokeq
  where
    tokeq :: Tok.LToken -> Maybe Tok.LToken
    tokeq t = if f t then Just t else Nothing

nextPos :: SourcePos -> Tok.LToken -> [Tok.LToken] -> SourcePos
nextPos pos _ _ = pos

-- | Parses given LToken
tok :: (Stream [Tok.LToken] m Tok.LToken) => Tok.LToken -> ParsecT [Tok.LToken] u m Tok.LToken
tok t = satisfy (==t) <?> show t

tok' :: (Stream [Tok.LToken] m Tok.LToken) => Tok.LToken -> ParsecT [Tok.LToken] u m ()
tok' t = tok t >> return ()

-- | Parses a LTokIdent
anyIdent :: Monad m => ParsecT [Tok.LToken] u m Tok.LToken
anyIdent = satisfy p <?> "identifier"
  where p t = case t of Tok.LTokIdent s -> True
                        _ -> False

ident :: Monad m => String -> ParsecT [Tok.LToken] u m Tok.LToken
ident s = satisfy p <?> "identifier"
  where p t = case t of (Tok.LTokIdent (Tok.LIdentSimpleToken s')) ->
                          (fmap toLower s) ==  (fmap toLower s')
                        (Tok.LTokIdent (Tok.LIdentQualifiedToken s' s'')) ->
                          (fmap toLower s) == (fmap toLower s' ++ "." ++ fmap toLower s'')
                        (Tok.LTokIdent (Tok.LIdentDoubleQualifiedToken s' s'' s''')) ->
                          (fmap toLower s) == (fmap toLower s' ++ "." ++
                                               fmap toLower s'' ++ "." ++
                                               fmap toLower s''')
                        _ -> False

-- | Parses a LTokNum
number :: Monad m => ParsecT [Tok.LToken] u m Tok.LToken
number = satisfy p <?> "number"
  where p t = case t of Tok.LTokNum n -> True
                        _ -> False

-- | Parses a LTokStr
stringlit :: Monad m => ParsecT [Tok.LToken] u m Tok.LToken
stringlit = satisfy p <?> "string"
  where p t = case t of Tok.LTokStr s -> True
                        _ -> False

symbolicE :: Monad m => ParsecT [Tok.LToken] u m Tok.LToken
symbolicE = satisfy p <?> "symbolic"
  where p t = case t of Tok.LTokSymbolic n -> True
                        _ -> False

identConvert :: Tok.LToken -> Syn.Ident
identConvert (Tok.LTokIdent ident) =
  case ident of
    Tok.LIdentSimpleToken s -> Syn.SimpleIdent s
    Tok.LIdentQualifiedToken s1 s2 -> Syn.QualifiedIdent s1 s2
    Tok.LIdentDoubleQualifiedToken s1 s2 s3 -> Syn.DoubleQualifiedIdent s1 s2 s3

-- Expressions
--
nullExpr :: Parser Syn.SimpleExpr
nullExpr = do
  tok' Tok.LTokNull
  return $ Syn.Lit $ Syn.NullLiteral

boolExpr :: Parser Syn.SimpleExpr
boolExpr = do
  tOrF <- tok Tok.LTokTrue <|> tok Tok.LTokFalse
  return $ Syn.Lit $ Syn.BLit (tOrF == Tok.LTokTrue)

numberExpr :: Parser Syn.SimpleExpr
numberExpr = do
  (Tok.LTokNum n) <- number
  return $ Syn.Lit $ Syn.NLit n

stringExpr :: Parser Syn.SimpleExpr
stringExpr = do
  (Tok.LTokStr s) <- stringlit
  return $ Syn.Lit $ Syn.SLit s

litExpr :: Parser Syn.SimpleExpr
litExpr = choice [ nullExpr, boolExpr, numberExpr, stringExpr ]

identExpr :: Parser Syn.SimpleExpr
identExpr = do
  s <- identConvert <$> anyIdent
  return $ Syn.Ident s

-- | Simple Expression Operator Precedence
-- BINARY, COLLATE,
-- !
-- - (unary minus), ~ (unary bit inversion)
-- + (should I ignore this?)
-- ||

simpleExprTable :: [[ParExp.Operator [Tok.LToken] () Identity Syn.SimpleExpr]]
simpleExprTable = [ [ParExp.Prefix simpleExprNot],
                    [ParExp.Prefix simpleExprMinus, ParExp.Prefix simpleExprInv],
                    [ParExp.Prefix simpleExprPlus],
                    [ParExp.Infix simpleExprOr ParExp.AssocLeft]
                  ]

simpleExprPlus :: Parser (Syn.SimpleExpr -> Syn.SimpleExpr)
simpleExprPlus = tok' Tok.LTokPlus *> pure Syn.SEPlus

simpleExprMinus :: Parser (Syn.SimpleExpr -> Syn.SimpleExpr)
simpleExprMinus = tok' Tok.LTokMinus *> pure Syn.SEMinus

simpleExprInv :: Parser (Syn.SimpleExpr -> Syn.SimpleExpr)
simpleExprInv = tok' Tok.LTokBitInv *> pure Syn.SETilde

simpleExprNot :: Parser (Syn.SimpleExpr -> Syn.SimpleExpr)
simpleExprNot = tok' Tok.LTokNotOp *> pure Syn.SENot
  
simpleExprOr :: Parser (Syn.SimpleExpr -> Syn.SimpleExpr -> Syn.SimpleExpr)
simpleExprOr = do
  tok' Tok.LTokOrOp
  return Syn.SEOr

simpleExprList :: Parser Syn.SimpleExpr
simpleExprList = do
  tok' Tok.LTokOpenPar
  exprs <- sepBy1 parseExpr (tok' Tok.LTokComma)
  tok' Tok.LTokClosePar
  return $ Syn.SEList exprs

symbolicExpr :: Parser Syn.SimpleExpr
symbolicExpr = do
  (Tok.LTokSymbolic n) <- symbolicE
  return $ Syn.SymbolicE n  

simpleExprTerm :: Parser Syn.SimpleExpr
simpleExprTerm = choice [ litExpr, identExpr, simpleExprList, symbolicExpr ]

simpleExpr :: Parser Syn.SimpleExpr
simpleExpr = ParExp.buildExpressionParser simpleExprTable simpleExprTerm

-- | Bit Expression Operator Precedence
-- ^
-- *, /, DIV, MOD, %
-- +, -
-- <<, >>
-- &
-- |

bitExprTable :: [[ParExp.Operator [Tok.LToken] () Identity Syn.BitExpr]]
bitExprTable = [ [ParExp.Infix bitExprXOr ParExp.AssocLeft],
                 [ParExp.Infix bitExprMul ParExp.AssocLeft,
                  ParExp.Infix bitExprDiv ParExp.AssocLeft,
                  ParExp.Infix bitExprIntDiv ParExp.AssocLeft,
                  ParExp.Infix bitExprMod ParExp.AssocLeft],
                 [ParExp.Infix bitExprAdd ParExp.AssocLeft,
                  ParExp.Infix bitExprSub ParExp.AssocLeft],
                 [ParExp.Infix bitExprLShift ParExp.AssocLeft,
                  ParExp.Infix bitExprRShift ParExp.AssocLeft],
                 [ParExp.Infix bitExprAnd ParExp.AssocLeft],
                 [ParExp.Infix bitExprOr ParExp.AssocLeft]
               ]

bitExprOr :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
bitExprOr = tok' Tok.LTokBitOr *> pure Syn.BitOr

bitExprAnd :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
bitExprAnd = tok' Tok.LTokBitAnd *> pure Syn.BitAnd

bitExprLShift :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
bitExprLShift = tok' Tok.LTokLShift *> pure Syn.BitLShift

bitExprRShift :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
bitExprRShift = tok' Tok.LTokRShift *> pure Syn.BitRShift

bitExprAdd :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
bitExprAdd = tok' Tok.LTokPlus *> pure Syn.BitAdd

bitExprSub :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
bitExprSub = tok' Tok.LTokMinus *> pure Syn.BitSub

bitExprMul :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
bitExprMul = tok' Tok.LTokMul *> pure Syn.BitMul

bitExprDiv :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
bitExprDiv = tok' Tok.LTokDiv *> pure Syn.BitDiv

bitExprIntDiv :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
bitExprIntDiv = tok' Tok.LTokIntDiv *> pure Syn.BitIntDiv

bitExprMod :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
bitExprMod = tok' Tok.LTokMod *> pure Syn.BitMod

bitExprXOr :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
bitExprXOr = tok' Tok.LTokBitXOr *> pure Syn.BitXOr

bitExprTerm :: Parser Syn.BitExpr
bitExprTerm = Syn.SimpleExpr <$> simpleExpr

bitExpr :: Parser Syn.BitExpr
bitExpr = ParExp.buildExpressionParser bitExprTable bitExprTerm

-- | Predicate Expressions
--
predInExprList :: Parser Syn.Predicate
predInExprList = do
  e1 <- bitExpr
  tok' Tok.LTokIn
  e2 <- simpleExprList
  return $ Syn.PredInExprList e1 e2

predNotInExprList :: Parser Syn.Predicate
predNotInExprList = do
  e1 <- bitExpr
  tok' Tok.LTokNot
  tok' Tok.LTokIn
  e2 <- simpleExprList
  return $ Syn.PredNotInExprList e1 e2

predTerm :: Parser Syn.Predicate
predTerm = Syn.BitExpr <$> bitExpr

predExpr :: Parser Syn.Predicate
predExpr = try predInExprList
           <|> try predNotInExprList
           <|> predTerm

-- | Boolean Primary Expressions
-- 
-- Operator Precedences
-- =, <=>, >=, >, <=, <, <>, !=, IS
-- NOT

-- boolean_primary -> predicate boolean_primary'
-- boolean_primary' -> IS [NOT] NULL boolean_primary'
--                  -> <=> predicate boolean_primary'
--                  -> comparison_operator predicate boolean_primary'
--                  -> comparison_operator {ALL | ANY} (subquery)
--                  -> nil

boolPExpr :: Parser Syn.BooleanPrimary
boolPExpr = do
  p <- predExpr
  bp <- boolPExpr'
  case bp of
    Just xs -> foldM (\p' (f,x) -> return $ f p' x) (Syn.Predicate p) xs
    Nothing -> return $ Syn.Predicate p

boolPExpr' = Just <$> try (boolComp Tok.LTokEq Syn.BPEq)
             <|> Just <$> try (boolComp Tok.LTokSafeNotEq Syn.BPSafeNotEq)
             <|> Just <$> try (boolComp Tok.LTokGTE Syn.BPGTE)
             <|> Just <$> try (boolComp Tok.LTokGT Syn.BPGT)
             <|> Just <$> try (boolComp Tok.LTokLTE Syn.BPLTE)
             <|> Just <$> try (boolComp Tok.LTokLT Syn.BPLT)
             <|> Just <$> try (boolComp Tok.LTokNotEq Syn.BPNotEq)
             <|> return Nothing

boolComp :: Tok.LToken
         -> (Syn.BooleanPrimary -> Syn.Predicate -> Syn.BooleanPrimary)
         -> Parser [(Syn.BooleanPrimary -> Syn.Predicate -> Syn.BooleanPrimary, Syn.Predicate)]
boolComp t ctor = do
  tok' t
  p <- predExpr
  bp <- boolPExpr'
  case bp of
    Just xs -> return $ (ctor, p) : xs
    Nothing -> return $ [(ctor, p)]
  

-- | Expression Operator Precedence
-- !
-- NOT
-- AND, &&
-- XOR
-- OR, ||

exprTable :: [[ParExp.Operator [Tok.LToken] () Identity Syn.Expr]]
exprTable = [ [ParExp.Prefix exprNot],
              [ParExp.Infix exprAnd ParExp.AssocLeft],
              [ParExp.Infix exprXOr ParExp.AssocLeft],
              [ParExp.Infix exprOr ParExp.AssocLeft]
            ]

exprOr :: Parser (Syn.Expr -> Syn.Expr -> Syn.Expr)
exprOr = (try (tok' Tok.LTokOr) <|> (tok' Tok.LTokOrOp)) *> pure Syn.EOr

exprXOr :: Parser (Syn.Expr -> Syn.Expr -> Syn.Expr)
exprXOr = tok' Tok.LTokXOr *> pure Syn.EXOr

exprAnd :: Parser (Syn.Expr -> Syn.Expr -> Syn.Expr)
exprAnd = (try (tok' Tok.LTokAnd) <|> (tok' Tok.LTokAndOp)) *> pure Syn.EAnd

exprNot :: Parser (Syn.Expr -> Syn.Expr)
exprNot = (try (tok' Tok.LTokNot) <|> (tok' Tok.LTokNotOp)) *> pure Syn.ENot

exprIsExpr :: Parser Syn.Expr
exprIsExpr = do
  bp <- boolPExpr
  tok' Tok.LTokIs
  tOrF <- tok Tok.LTokTrue <|> tok Tok.LTokFalse
  return $ Syn.BIs bp (tOrF == Tok.LTokTrue)
  
exprIsNotExpr :: Parser Syn.Expr
exprIsNotExpr = do
  bp <- boolPExpr
  tok' Tok.LTokIs
  tok' Tok.LTokNot
  tOrF <- tok Tok.LTokTrue <|> tok Tok.LTokFalse
  return $ Syn.BIsNot bp (tOrF == Tok.LTokTrue)

exprTerm :: Parser Syn.Expr
exprTerm = try exprIsExpr
           <|> try exprIsNotExpr
           <|> Syn.BooleanPrimary <$> boolPExpr

parseExpr :: Parser Syn.Expr
parseExpr = ParExp.buildExpressionParser exprTable exprTerm


-- Statements
--
getDataTypeLength :: Parser Integer
getDataTypeLength = do
  tok' Tok.LTokOpenPar
  (Tok.LTokNum n) <- number
  tok' Tok.LTokClosePar
  return $ read n
  
bitDataType :: Parser Syn.DataType
bitDataType = do
  t <- ident "bit"
  len <- try getDataTypeLength <|> pure 0
  return $ Syn.DTypeBit $ Just len

tinyIntDataType :: Parser Syn.DataType
tinyIntDataType = do
  t <- tok' Tok.LTokTinyInt
  len <- try getDataTypeLength <|> pure 0
  return $ Syn.DTypeTinyInt $ Just len

smallIntDataType :: Parser Syn.DataType
smallIntDataType = do
  t <- tok' Tok.LTokSmallInt
  len <- try getDataTypeLength <|> pure 0
  return $ Syn.DTypeSmallInt $ Just len

mediumIntDataType :: Parser Syn.DataType
mediumIntDataType = do
  t <- tok' Tok.LTokMediumInt
  len <- try getDataTypeLength <|> pure 0
  return $ Syn.DTypeMediumInt $ Just len

intDataType :: Parser Syn.DataType
intDataType = do
  t <- tok' Tok.LTokInt
  len <- try getDataTypeLength <|> pure 0
  return $ Syn.DTypeInt $ Just len

integerDataType :: Parser Syn.DataType
integerDataType = do
  t <- tok' Tok.LTokInteger
  len <- try getDataTypeLength <|> pure 0
  return $ Syn.DTypeInteger $ Just len

bigIntDataType :: Parser Syn.DataType
bigIntDataType = do
  t <- tok' Tok.LTokBigInt
  len <- try getDataTypeLength <|> pure 0
  return $ Syn.DTypeBigInt $ Just len

charDataType :: Parser Syn.DataType
charDataType = do
  t <- tok' Tok.LTokChar
  len <- try getDataTypeLength <|> pure 0
  return $ Syn.DTypeChar $ Just len

varCharDataType :: Parser Syn.DataType
varCharDataType = do
  t <- tok' Tok.LTokVarChar
  len <- try getDataTypeLength <|> pure 0
  return $ Syn.DTypeVarChar $ Just len

dataType :: Parser Syn.DataType
dataType = try bitDataType
           <|> try tinyIntDataType
           <|> try smallIntDataType
           <|> try mediumIntDataType
           <|> try intDataType
           <|> try integerDataType
           <|> try bigIntDataType
           <|> try charDataType
           <|> varCharDataType

fieldDef :: Parser Syn.ColumnDefinition
fieldDef = do
  t <- dataType
  nullable <- try (tok' Tok.LTokNot >> tok' Tok.LTokNull *> pure False)
              <|> try (tok' Tok.LTokNull *> pure True)
              <|> return True
  autoIncrement <- try (ident "auto_increment" *> pure True) <|> return False
  unique <- try (tok' Tok.LTokUnique >> tok' Tok.LTokKey *> pure True)
            <|> try (tok' Tok.LTokUnique *> pure True)
            <|> return False
  primary <- try (tok' Tok.LTokPrimary >> tok' Tok.LTokKey *> pure True)
             <|> try (tok' Tok.LTokKey *> pure True)
             <|> return False
  return $ Syn.FieldDef { Syn.fieldType = t
                        , Syn.nullable = nullable
                        , Syn.autoIncrement = autoIncrement
                        , Syn.uniqueKey = unique
                        , Syn.primaryKey = primary
                        }

refDef :: Parser Syn.RefDefinition
refDef = do
  tok' Tok.LTokReferences
  s <- identConvert <$> anyIdent
  tok' Tok.LTokOpenPar
  colNameIdents <- sepBy1 anyIdent (tok' Tok.LTokComma)
  return Syn.RefDefinition { Syn.refDefTblName = s
                           , Syn.refDefColNames = getNames colNameIdents
                           }
    where getNames = fmap identConvert

columnDef :: Parser Syn.CreateDefinition
columnDef = do
  name <- identConvert <$> anyIdent
  def <- fieldDef
  refDefs <- optionMaybe (try refDef)
  return Syn.ColumnDef { Syn.name = name
                       , Syn.definition = def
                       , Syn.colDefRefDef = refDefs
                       }

pkDef :: Parser Syn.CreateDefinition
pkDef = do
  tok' Tok.LTokPrimary
  tok' Tok.LTokKey
  tok' Tok.LTokOpenPar
  colNameIdents <- sepBy1 anyIdent (tok' Tok.LTokComma)
  tok' Tok.LTokClosePar
  return Syn.PKDef { Syn.pkColNames = getNames colNameIdents }
    where getNames = fmap identConvert

keyDef :: Parser Syn.CreateDefinition
keyDef = do
  tok' Tok.LTokKey
  idxName <- optionMaybe (try anyIdent)
  tok' Tok.LTokOpenPar
  colNameIdents <- sepBy1 anyIdent (tok' Tok.LTokComma)
  tok' Tok.LTokClosePar
  return Syn.KeyDef { Syn.keyColNames = getNames colNameIdents }
    where getNames = fmap identConvert

ukDef :: Parser Syn.CreateDefinition
ukDef = do
  tok' Tok.LTokUnique
  tok' Tok.LTokKey
  idxName <- optionMaybe (try anyIdent)  
  tok' Tok.LTokOpenPar
  colNameIdents <- sepBy1 anyIdent (tok' Tok.LTokComma)
  tok' Tok.LTokClosePar
  return Syn.UKDef { Syn.ukColNames = getNames colNameIdents }
    where getNames = fmap identConvert

fkDef :: Parser Syn.CreateDefinition
fkDef = do
  tok' Tok.LTokForeign
  tok' Tok.LTokKey
  idxName <- optionMaybe (try anyIdent)
  tok' Tok.LTokOpenPar
  colNameIdents <- sepBy1 anyIdent (tok' Tok.LTokComma)
  tok' Tok.LTokClosePar
  refDefs <- refDef
  return Syn.FKDef { Syn.fkColNames = getNames colNameIdents
                   , Syn.fkRefDef = refDefs
                   }
    where getNames = fmap identConvert

checkExpr :: Parser Syn.CreateDefinition
checkExpr = do
  tok' Tok.LTokCheck
  tok' Tok.LTokOpenPar
  expr <- parseExpr
  tok' Tok.LTokClosePar
  return Syn.CheckExpr { Syn.checkExpr = expr }

createDef :: Parser Syn.CreateDefinition
createDef = try columnDef
            <|> try pkDef
            <|> try keyDef
            <|> try ukDef
            <|> try fkDef
            <|> checkExpr

createTableStmt :: Parser Syn.CreateTableStmt
createTableStmt = do
  tok' Tok.LTokCreate
  temp <- try (ident "temporary" *> pure True) <|> pure False
  tok' Tok.LTokTable
  name <- identConvert <$> anyIdent
  tok' Tok.LTokOpenPar
  defs <- sepBy1 createDef (tok' Tok.LTokComma)
  tok' Tok.LTokClosePar
  return Syn.CreateTable { Syn.isTemporary = temp
                         , Syn.tblName = name
                         , Syn.createDefinitions = defs
                         }

-- Select Stmts
--
selectExpr :: Parser [Syn.Expr]
selectExpr = do
  e <- parseExpr
  es <- many (tok' Tok.LTokComma *> parseExpr)
  return $ e:es

fromExpr :: Parser Syn.TableReferences
fromExpr = tok' Tok.LTokFrom *> tableReferences

whereExpr :: Parser Syn.Expr
whereExpr = tok' Tok.LTokWhere *> parseExpr

joinExpr :: Parser Syn.JoinCondition
joinExpr = do
  tok' Tok.LTokOn
  e <- parseExpr
  return $ Syn.JoinExpr { Syn.joinExpr = e }

-- tableReferences :: Parser Syn.TableReferences
-- tableReferences = do
--   ref <- tableReference
--   refs <- many (tok' Tok.LTokComma *> tableReference)
--   return $ Syn.TableReferences { Syn.tableReferences = (ref:refs) }

tableReferences :: Parser Syn.TableReferences
tableReferences = do
  refs <- sepBy1 tableReference (tok' Tok.LTokComma)
  return $ Syn.TableReferences { Syn.tableReferences = refs }

{-|

Original Grammar:

table_reference:
    table_factor
  | join_table

table_factor:
    tbl_name [PARTITION (partition_names)]
        [[AS] alias] [index_hint_list]
  | table_subquery [AS] alias
  | ( table_references )

join_table:
    table_reference [INNER | CROSS] JOIN table_factor [join_condition]
  | table_reference STRAIGHT_JOIN table_factor
  | table_reference STRAIGHT_JOIN table_factor ON conditional_expr
  | table_reference {LEFT|RIGHT} [OUTER] JOIN table_reference join_condition
  | table_reference NATURAL [{LEFT|RIGHT} [OUTER]] JOIN table_factor

After getting rid of Left-Recursion:

table_reference:
    table_factor table_reference'

table_reference':
    join_table table_reference'
  | nil

join_table:
    [INNER | CROSS] JOIN table_factor [join_condition]
  | STRAIGHT_JOIN table_factor
  | STRAIGHT_JOIN table_factor ON conditional_expr
  | {LEFT|RIGHT} [OUTER] JOIN table_reference join_condition
  | NATURAL [{LEFT|RIGHT} [OUTER]] JOIN table_factor

|-}

tableReference :: Parser Syn.TableReference
tableReference = do
  tfactor <- tableFactor
  joins <- many joinTable
  return $ Syn.TableReference { Syn.tableFactor = tfactor
                              , Syn.joinTables = joins
                              }

tableFactorTableName :: Parser Syn.TableFactor
tableFactorTableName = do
  name <- identConvert <$> anyIdent
  return $ Syn.TableFactor { Syn.tableFactorName = name                           
                           }

tableFactorTableReferences :: Parser Syn.TableFactor
tableFactorTableReferences = do
  tok' Tok.LTokOpenPar
  refs <- tableReferences
  tok' Tok.LTokClosePar
  return $ Syn.TableFactors { Syn.tableFactors = refs }

tableFactor :: Parser Syn.TableFactor
tableFactor = try tableFactorTableName
              <|> tableFactorTableReferences

innerJoin :: Parser Syn.JoinTable
innerJoin = do
  optional $ try (tok' Tok.LTokInner)
  optional $ try (tok' Tok.LTokCross)
  tok' Tok.LTokJoin
  tfactor <- tableFactor
  joinCond <- optionMaybe joinCondition
  return $ Syn.InnerJoin { Syn.innerTableFactor = tfactor
                         , Syn.innerJoinConds = joinCond
                         }

straightJoin :: Parser Syn.JoinTable
straightJoin = do
  tok' Tok.LTokStraightJoin
  tfactor <- tableFactor
  return $ Syn.StraightJoin { Syn.straightTableFactor = tfactor
                            }

outerJoin :: Parser Syn.JoinTable
outerJoin = do
  optional $ try (tok' Tok.LTokLeft)
  isLeft <- option True (try (tok' Tok.LTokRight *> return False))
  optional $ try (tok' Tok.LTokOuter)
  tok' Tok.LTokJoin
  joinTableRef <- tableReference
  joinCond <- joinCondition
  return $ Syn.OuterJoin { Syn.outerLeft = isLeft
                         , Syn.outerJoinTableRef = joinTableRef
                         , Syn.outerJoinCond = joinCond
                         }

joinTable :: Parser Syn.JoinTable
joinTable = try innerJoin
            <|> try straightJoin
            <|> outerJoin

joinCondition :: Parser Syn.JoinCondition
joinCondition = tok' Tok.LTokOn *> return Syn.JoinExpr <*> parseExpr

parseSelect :: Parser Syn.SelectStmt
parseSelect = do
  tok' Tok.LTokSelect
  isAll <- try (tok' Tok.LTokAll *> return True)
           <|> return False
  isDistinct <- try (tok' Tok.LTokDistinct *> return True)
                <|> return False
  sexprs <- selectExpr
  tableRefs <- Just <$> try fromExpr <|> return Nothing
  case tableRefs of
    Just x -> do
      whereCond <- Just <$> try whereExpr <|> return Nothing
      return $ Syn.Select { Syn.selectAll = isAll
                          , Syn.selectDistinct = isDistinct
                          , Syn.selectExprs = sexprs
                          , Syn.selectTabRefs = tableRefs
                          , Syn.selectWhereCond = whereCond
                          }
    Nothing ->
      return $ Syn.Select { Syn.selectAll = isAll
                          , Syn.selectDistinct = isDistinct
                          , Syn.selectExprs = sexprs
                          , Syn.selectTabRefs = tableRefs
                          , Syn.selectWhereCond = Nothing
                          }

-- Insert Stmts
--
parseInsert :: Parser Syn.InsertStmt
parseInsert = do
  tok' Tok.LTokInsert
  try (tok' Tok.LTokInto) <|> pure ()
  tblName <- identConvert <$> anyIdent
  colNames <- Just <$> try insertColNames <|> pure Nothing
  tok' Tok.LTokValues
  colValues <- insertColValues
  return $ Syn.Insert { Syn.insertTblName = tblName
                      , Syn.insertColNames = colNames
                      , Syn.insertValues = colValues
                      }

insertColNames :: Parser [Syn.Ident]
insertColNames = do
  tok' Tok.LTokOpenPar
  colNames <- sepBy1 (identConvert <$> anyIdent) (tok' Tok.LTokComma)
  tok' Tok.LTokClosePar
  return colNames

insertColValues :: Parser [Syn.Expr]
insertColValues = do
  tok' Tok.LTokOpenPar
  colValues <- sepBy1 parseExpr (tok' Tok.LTokComma)
  tok' Tok.LTokClosePar
  return colValues

-- Update Stmts
--
parseUpdate :: Parser Syn.UpdateStmt
parseUpdate = do
  tok' Tok.LTokUpdate
  tblRef <- tableReference
  tok' Tok.LTokSet
  colNameValues <- sepBy1 updateColNameValue (tok' Tok.LTokComma)
  whereCond <- Just <$> try whereExpr <|> return Nothing
  return $ Syn.Update { Syn.updateTblRef = tblRef
                      , Syn.updateColNameValues = colNameValues
                      , Syn.updateWhereCond = whereCond
                      }

updateColNameValue :: Parser (Syn.Ident, Syn.Expr)
updateColNameValue = do
  colName <- identConvert <$> anyIdent
  tok' Tok.LTokEq
  colValue <- parseExpr
  return (colName, colValue)

-- Delete Stmts
--
parseDelete :: Parser Syn.DeleteStmt
parseDelete = do
  tok' Tok.LTokDelete
  tok' Tok.LTokFrom
  tblName <- identConvert <$> anyIdent
  whereCond <- Just <$> try whereExpr <|> return Nothing
  return $ Syn.Delete { Syn.deleteTblName = tblName
                      , Syn.deleteWhereCond = whereCond
                      }
