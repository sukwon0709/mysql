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
  exprs <- sepBy1 simpleExpr (tok' Tok.LTokComma)
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

predExpr :: Parser Syn.Predicate
predExpr = Syn.BitExpr <$> bitExpr

boolPredSafeNotEq :: Parser (Syn.BooleanPrimary -> Syn.BooleanPrimary -> Syn.BooleanPrimary)
boolPredSafeNotEq = do
  tok' Tok.LTokSafeNotEq
  return Syn.BPSafeNotEq

boolPredEq :: Parser (Syn.BooleanPrimary -> Syn.BooleanPrimary -> Syn.BooleanPrimary)
boolPredEq = do
  tok' Tok.LTokEq
  return Syn.BPEq

boolPredGte :: Parser (Syn.BooleanPrimary -> Syn.BooleanPrimary -> Syn.BooleanPrimary)
boolPredGte = do
  tok' Tok.LTokGTE
  return Syn.BPGte

boolPredGt :: Parser (Syn.BooleanPrimary -> Syn.BooleanPrimary -> Syn.BooleanPrimary)
boolPredGt = do
  tok' Tok.LTokGT
  return Syn.BPGt

boolPredLte :: Parser (Syn.BooleanPrimary -> Syn.BooleanPrimary -> Syn.BooleanPrimary)
boolPredLte = do
  tok' Tok.LTokLTE
  return Syn.BPLte

boolPredLt :: Parser (Syn.BooleanPrimary -> Syn.BooleanPrimary -> Syn.BooleanPrimary)
boolPredLt = do
  tok' Tok.LTokLT
  return Syn.BPLt

boolPredNotEq :: Parser (Syn.BooleanPrimary -> Syn.BooleanPrimary -> Syn.BooleanPrimary)
boolPredNotEq = do
  tok' Tok.LTokNotEq
  return Syn.BPNotEq

boolPredBinOp :: Parser (Syn.BooleanPrimary -> Syn.BooleanPrimary -> Syn.BooleanPrimary)
boolPredBinOp = boolPredSafeNotEq
                <|> boolPredEq
                <|> boolPredGte
                <|> boolPredGt
                <|> boolPredLte
                <|> boolPredLt
                <|> boolPredNotEq

boolPredTerm :: Parser Syn.BooleanPrimary
boolPredTerm = Syn.Predicate <$> predExpr

boolPredExpr :: Parser Syn.BooleanPrimary
boolPredExpr = boolPredTerm `chainl1` boolPredBinOp

exprOr :: Parser (Syn.Expr -> Syn.Expr -> Syn.Expr)
exprOr = do
  tok' Tok.LTokOr
  return Syn.EOr

exprXOr :: Parser (Syn.Expr -> Syn.Expr -> Syn.Expr)
exprXOr = do
  tok' Tok.LTokXOr
  return Syn.EXOr

exprAnd :: Parser (Syn.Expr -> Syn.Expr -> Syn.Expr)
exprAnd = do
  tok' Tok.LTokAnd
  return Syn.EAnd

exprBinOp :: Parser (Syn.Expr -> Syn.Expr -> Syn.Expr)
exprBinOp = exprOr
            <|> exprXOr
            <|> exprAnd

exprNot :: Parser (Syn.Expr -> Syn.Expr)
exprNot = do
  tok' Tok.LTokNot
  return Syn.ENot

exprUOp :: Parser (Syn.Expr -> Syn.Expr)
exprUOp = exprNot

exprIsExpr :: Parser Syn.Expr
exprIsExpr = do
  bp <- boolPredExpr
  tok' Tok.LTokIs
  tOrF <- tok Tok.LTokTrue <|> tok Tok.LTokFalse
  return $ Syn.BIs bp (tOrF == Tok.LTokTrue)
  
exprIsNotExpr :: Parser Syn.Expr
exprIsNotExpr = do
  bp <- boolPredExpr
  tok' Tok.LTokIs
  tok' Tok.LTokNot
  tOrF <- tok Tok.LTokTrue <|> tok Tok.LTokFalse
  return $ Syn.BIsNot bp (tOrF == Tok.LTokTrue)

exprTerm :: Parser Syn.Expr
exprTerm = try exprIsExpr
           <|> try exprIsNotExpr
           <|> Syn.BooleanPrimary <$> boolPredExpr

parseExpr :: Parser Syn.Expr
parseExpr = exprTerm `chainl1` exprBinOp
            <|> exprUOp <*> parseExpr
