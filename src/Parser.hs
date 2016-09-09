{-# LANGUAGE FlexibleContexts #-}
module Parser where

import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import Text.Parsec.Pos (SourcePos)
import qualified Text.Parsec.Expr as ParExp

import qualified Lexer                  as Lex
import qualified Token                  as Tok
import qualified Syntax as Syn
import Data.Functor.Identity
import Data.Char (toLower)


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
  where p t = case t of Tok.LTokIdent s' -> (fmap toLower s) ==  (fmap toLower s')
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
  Tok.LTokIdent s <- anyIdent
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
simpleExprNot = tok' Tok.LTokNot *> pure Syn.SENot
  
simpleExprOr :: Parser (Syn.SimpleExpr -> Syn.SimpleExpr -> Syn.SimpleExpr)
simpleExprOr = do
  tok' Tok.LTokOr
  return Syn.SEOr

simpleExprList :: Parser Syn.SimpleExpr
simpleExprList = do
  tok' Tok.LTokOpenPar
  exprs <- sepBy1 parseExpr (tok' Tok.LTokComma)
  tok' Tok.LTokClosePar
  return $ Syn.SEList exprs

simpleExprTerm :: Parser Syn.SimpleExpr
simpleExprTerm = choice [ litExpr, identExpr, simpleExprList ]

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

boolTerm :: Parser Syn.BooleanPrimary
boolTerm = Syn.Predicate <$> predExpr

chainl1' :: Parser a -> Parser (b -> a -> b) -> (a -> b) -> Parser b
chainl1' p op ctor = p >>= (rest . ctor)
  where rest ctx =
          do
            f <- op
            y <- p
            rest $ f ctx y
          <|> return ctx

boolSafeNotEqOp :: Parser (Syn.BooleanPrimary -> Syn.Predicate -> Syn.BooleanPrimary)
boolSafeNotEqOp = tok' Tok.LTokSafeNotEq *> pure Syn.BPSafeNotEq

boolSafeNotEq :: Parser Syn.BooleanPrimary
boolSafeNotEq = chainl1' predExpr boolSafeNotEqOp Syn.Predicate

boolEqOp :: Parser (Syn.BooleanPrimary -> Syn.Predicate -> Syn.BooleanPrimary)
boolEqOp = tok' Tok.LTokEq *> pure Syn.BPEq

boolEq :: Parser Syn.BooleanPrimary
boolEq = chainl1' predExpr boolEqOp Syn.Predicate

boolGTEOp :: Parser (Syn.BooleanPrimary -> Syn.Predicate -> Syn.BooleanPrimary)
boolGTEOp = tok' Tok.LTokGTE *> pure Syn.BPGte

boolGTE :: Parser Syn.BooleanPrimary
boolGTE = chainl1' predExpr boolGTEOp Syn.Predicate

boolGTOp :: Parser (Syn.BooleanPrimary -> Syn.Predicate -> Syn.BooleanPrimary)
boolGTOp = tok' Tok.LTokGT *> pure Syn.BPGt

boolGT :: Parser Syn.BooleanPrimary
boolGT = chainl1' predExpr boolGTOp Syn.Predicate

boolLTEOp :: Parser (Syn.BooleanPrimary -> Syn.Predicate -> Syn.BooleanPrimary)
boolLTEOp = tok' Tok.LTokLTE *> pure Syn.BPLte

boolLTE :: Parser Syn.BooleanPrimary
boolLTE = chainl1' predExpr boolLTEOp Syn.Predicate

boolLTOp :: Parser (Syn.BooleanPrimary -> Syn.Predicate -> Syn.BooleanPrimary)
boolLTOp = tok' Tok.LTokLT *> pure Syn.BPLt

boolLT :: Parser Syn.BooleanPrimary
boolLT = chainl1' predExpr boolLTOp Syn.Predicate

boolNotEqOp :: Parser (Syn.BooleanPrimary -> Syn.Predicate -> Syn.BooleanPrimary)
boolNotEqOp = tok' Tok.LTokNotEq *> pure Syn.BPNotEq

boolNotEq :: Parser Syn.BooleanPrimary
boolNotEq = chainl1' predExpr boolNotEqOp Syn.Predicate

boolPExpr :: Parser Syn.BooleanPrimary
boolPExpr = try boolSafeNotEq
            <|> try boolEq
            <|> try boolGTE
            <|> try boolGT
            <|> try boolLTE
            <|> try boolLT
            <|> try boolNotEq

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
exprOr = tok' Tok.LTokOr *> pure Syn.EOr

exprXOr :: Parser (Syn.Expr -> Syn.Expr -> Syn.Expr)
exprXOr = tok' Tok.LTokXOr *> pure Syn.EXOr

exprAnd :: Parser (Syn.Expr -> Syn.Expr -> Syn.Expr)
exprAnd = tok' Tok.LTokAnd *> pure Syn.EAnd

exprNot :: Parser (Syn.Expr -> Syn.Expr)
exprNot = tok' Tok.LTokNot *> pure Syn.ENot

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

columnDef :: Parser Syn.CreateDefinition
columnDef = do
  (Tok.LTokIdent name) <- anyIdent
  def <- fieldDef
  return Syn.ColumnDef { Syn.name = name
                       , Syn.definition = def
                       }

createTableStmt :: Parser Syn.CreateTableStmt
createTableStmt = do
  tok' Tok.LTokCreate
  temp <- try (ident "temporary" *> pure True) <|> pure False
  tok' Tok.LTokTable
  (Tok.LTokIdent name) <- anyIdent
  tok' Tok.LTokOpenPar
  defs <- sepBy1 columnDef (tok' Tok.LTokComma)
  tok' Tok.LTokClosePar
  return Syn.CreateTableStmt { Syn.isTemporary = temp
                             , Syn.tblName = name
                             , Syn.createDefinitions = defs
                             }
