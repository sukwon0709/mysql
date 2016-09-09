module ParserTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec.Prim

import qualified Lexer            as Lex
import           Parser
import qualified Syntax           as Syn
import qualified Token            as Tok


testCases :: [TestTree]
testCases = [ts1, ts2, ts3, ts4, ts5]

-- ts1 :: TestTree
-- ts1 = testCase "Create Table1" $ parseTest createTableStmt $
--   Lex.alexScanTokens $ "CREATE TABLE Scores (StudentID tinyint not null, \
--                        \ CourseID tinyint not null, \
--                        \ Points tinyint not null, \
--                        \ PRIMARY KEY (StudentID, CourseID), \
--                        \ CHECK(NOT(1 <= CourseID and CourseID <= 10) or Points < 6))"
ts1 :: TestTree
ts1 = testCase "Create Table1" $
  (parse createTableStmt ""
   (Lex.alexScanTokens $ "CREATE TABLE Students \
                         \ (StudentNr tinyint not null PRIMARY KEY, \
                         \ StudentName char(100) not null)"))
  @?= Right Syn.CreateTableStmt
  {
    Syn.isTemporary = False
  , Syn.tblName = "Students"
  , Syn.createDefinitions = [
      Syn.ColumnDef { Syn.name = "StudentNr"
                    , Syn.definition =
                        Syn.FieldDef { Syn.fieldType = Syn.DTypeTinyInt (Just 0)
                                     , Syn.nullable = False
                                     , Syn.autoIncrement = False
                                     , Syn.uniqueKey = False
                                     , Syn.primaryKey = True
                                     }
                    , Syn.colDefRefDef = Nothing
                    },
        Syn.ColumnDef { Syn.name = "StudentName"
                      , Syn.definition =
                          Syn.FieldDef { Syn.fieldType = Syn.DTypeChar (Just 100)
                                       , Syn.nullable = False
                                       , Syn.autoIncrement = False
                                       , Syn.uniqueKey = False
                                       , Syn.primaryKey = False
                                       }
                      , Syn.colDefRefDef = Nothing
                      }
      ]
  }

ts2 :: TestTree
ts2 = testCase "Expression1" $
  (parse parseExpr ""
   (Lex.alexScanTokens $ "1 <= CourseID and CourseID <= 10"))
  @?= Right (Syn.EAnd
  (Syn.BooleanPrimary
   (Syn.BPLTE
    (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "1")))))
    (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident "CourseID")))))
  (Syn.BooleanPrimary
   (Syn.BPLTE
    (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident "CourseID"))))
    (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "10")))))))

ts3 :: TestTree
ts3 = testCase "Expression2" $
  (parse parseExpr ""
   (Lex.alexScanTokens $ "NOT(1 <= CourseID and CourseID <= 10)"))
  @?= Right
  (Syn.ENot
    (Syn.BooleanPrimary
     (Syn.Predicate
      (Syn.BitExpr
       (Syn.SimpleExpr
        (Syn.SEList
          [Syn.EAnd
            (Syn.BooleanPrimary
              (Syn.BPLTE
                (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "1")))))
                (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident "CourseID")))))
            (Syn.BooleanPrimary
              (Syn.BPLTE
                (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident "CourseID"))))
                (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "10"))))))
          ]))))))

ts4 :: TestTree
ts4 = testCase "Expression3" $
  (parse parseExpr ""
    (Lex.alexScanTokens $ "NOT(1 <= CourseID and CourseID <= 10) or Points < 6"))
  @?= Right
  (Syn.EOr
    (Syn.ENot
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.SEList
                [Syn.EAnd
                  (Syn.BooleanPrimary
                    (Syn.BPLTE
                      (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "1")))))
                      (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident "CourseID")))))
                  (Syn.BooleanPrimary
                    (Syn.BPLTE
                      (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident "CourseID"))))
                      (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "10"))))))
                ]))))))
    (Syn.BooleanPrimary
      (Syn.BPLT
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident "Points"))))
        (Syn.BitExpr
          (Syn.SimpleExpr
            (Syn.Lit
              (Syn.NLit "6")))))))

ts5 :: TestTree
ts5 = testCase "Expression4" $
  (parse parseExpr ""
    (Lex.alexScanTokens $ "1 or Points < 6"))
  @?= Right
  (Syn.EOr
    (Syn.BooleanPrimary
      (Syn.Predicate
        (Syn.BitExpr
          (Syn.SimpleExpr
            (Syn.Lit
              (Syn.NLit "1"))))))
    (Syn.BooleanPrimary
      (Syn.BPLT
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident "Points"))))
        (Syn.BitExpr
          (Syn.SimpleExpr
            (Syn.Lit
              (Syn.NLit "6")))))))

