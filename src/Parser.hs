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

-- simpleExprUOp :: Parser (Syn.SimpleExpr -> Syn.SimpleExpr)
-- simpleExprUOp = simpleExprPlus
--                 <|> simpleExprMinus
--                 <|> simpleExprInv
--                 <|> simpleExprNot
  
simpleExprOr :: Parser (Syn.SimpleExpr -> Syn.SimpleExpr -> Syn.SimpleExpr)
simpleExprOr = do
  tok' Tok.LTokOr
  return Syn.SEOr

-- simpleExprBinOp :: Parser (Syn.SimpleExpr -> Syn.SimpleExpr -> Syn.SimpleExpr)
-- simpleExprBinOp = simpleExprOr

simpleExprList :: Parser Syn.SimpleExpr
simpleExprList = do
  tok' Tok.LTokOpenPar
  exprs <- sepBy1 parseExpr (tok' Tok.LTokComma)
  tok' Tok.LTokClosePar
  return $ Syn.SEList exprs

simpleExprTerm :: Parser Syn.SimpleExpr
simpleExprTerm = choice [ litExpr, identExpr, simpleExprList ]

-- simpleExpr :: Parser Syn.SimpleExpr
-- simpleExpr = simpleExprUOp <*> simpleExpr
--              <|> simpleExprTerm `chainl1` simpleExprBinOp

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

-- bitExprBinOp :: Parser (Syn.BitExpr -> Syn.BitExpr -> Syn.BitExpr)
-- bitExprBinOp = bitExprOr
--                <|> bitExprAnd
--                <|> bitExprLShift
--                <|> bitExprRShift
--                <|> bitExprAdd
--                <|> bitExprSub
--                <|> bitExprMul
--                <|> bitExprDiv
--                <|> bitExprIntDiv
--                <|> bitExprMod
--                <|> bitExprXOr

bitExprTerm :: Parser Syn.BitExpr
bitExprTerm = Syn.SimpleExpr <$> simpleExpr

-- bitExprSimpleExpr :: Parser Syn.BitExpr
-- bitExprSimpleExpr = Syn.SimpleExpr <$> simpleExpr

-- bitExpr :: Parser Syn.BitExpr
-- bitExpr = bitExprSimpleExpr `chainl1` bitExprBinOp

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

-- BP -> BP <=> P
-- BP -> P BP'
-- BP' -> <=> P BP'
-- BP' ->

boolTerm :: Parser Syn.BooleanPrimary
boolTerm = Syn.Predicate <$> predExpr

-- boolSafeNotEqExpr = do
--   tok' Tok.LTokSafeNotEq
--   t <- predExpr
--   e <- boolSafeNotEqExpr
--   return $ (flip Syn.BPSafeNotEq) t

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

-- exprBinOp :: Parser (Syn.Expr -> Syn.Expr -> Syn.Expr)
-- exprBinOp = exprOr
--             <|> exprXOr
--             <|> exprAnd

exprNot :: Parser (Syn.Expr -> Syn.Expr)
exprNot = tok' Tok.LTokNot *> pure Syn.ENot

-- exprUOp :: Parser (Syn.Expr -> Syn.Expr)
-- exprUOp = exprNot

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
