module ParserTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec.Prim
import           Text.Parsec.Error
import           Text.Parsec.Pos

import qualified MySQL.Lexer          as Lex
import           MySQL.ParserInternal
import qualified MySQL.Syntax         as Syn
import qualified MySQL.Token          as Tok


testCases :: [TestTree]
testCases = [ts1, ts2, ts3, ts4, ts5,
             ts6, ts7, ts8, ts9, ts10,
             ts11, ts12, ts13, ts14, ts15,
             ts16, ts17, ts18, ts19, ts20,
             ts21, ts22, ts23, ts24, ts25,
             ts26, ts27, ts28, ts29, ts30]

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
  @?= Right Syn.CreateTable
  {
    Syn.isTemporary = False
  , Syn.tblName = Syn.SimpleIdent "Students"
  , Syn.createDefinitions = [
      Syn.ColumnDef { Syn.name = Syn.SimpleIdent "StudentNr"
                    , Syn.definition =
                        Syn.FieldDef { Syn.fieldType = Syn.DTypeTinyInt (Just 0)
                                     , Syn.nullable = False
                                     , Syn.autoIncrement = False
                                     , Syn.uniqueKey = False
                                     , Syn.primaryKey = True
                                     }
                    , Syn.colDefRefDef = Nothing
                    },
        Syn.ColumnDef { Syn.name = Syn.SimpleIdent "StudentName"
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
    (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident (Syn.SimpleIdent "CourseID"))))))
  (Syn.BooleanPrimary
   (Syn.BPLTE
    (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident (Syn.SimpleIdent "CourseID")))))
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
                (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident (Syn.SimpleIdent "CourseID"))))))
            (Syn.BooleanPrimary
              (Syn.BPLTE
                (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident (Syn.SimpleIdent "CourseID")))))
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
                      (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident (Syn.SimpleIdent "CourseID"))))))
                  (Syn.BooleanPrimary
                    (Syn.BPLTE
                      (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident (Syn.SimpleIdent "CourseID")))))
                      (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "10"))))))
                ]))))))
    (Syn.BooleanPrimary
      (Syn.BPLT
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident (Syn.SimpleIdent "Points")))))
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
              (Syn.Ident (Syn.SimpleIdent "Points")))))
        (Syn.BitExpr
          (Syn.SimpleExpr
            (Syn.Lit
              (Syn.NLit "6")))))))

ts6 :: TestTree
ts6 = testCase "Check Expression1" $
  (parse checkExpr ""
    (Lex.alexScanTokens $ "CHECK(NOT(1 <= CourseID and CourseID <= 10) or Points < 6)"))
  @?= Right
  Syn.CheckExpr {
      Syn.checkExpr =
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
                              (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident (Syn.SimpleIdent "CourseID"))))))
                          (Syn.BooleanPrimary
                            (Syn.BPLTE
                              (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident (Syn.SimpleIdent "CourseID")))))
                              (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "10"))))))
                        ]))))))
            (Syn.BooleanPrimary
              (Syn.BPLT
                (Syn.Predicate
                  (Syn.BitExpr
                    (Syn.SimpleExpr
                      (Syn.Ident (Syn.SimpleIdent "Points")))))
                (Syn.BitExpr
                  (Syn.SimpleExpr
                    (Syn.Lit
                      (Syn.NLit "6")))))))
      }

ts7 :: TestTree
ts7 = testCase "ColumnDef1" $
  (parse createDef ""
    (Lex.alexScanTokens $ "StudentID tinyint not null"))
  @?= Right Syn.ColumnDef
  {
    Syn.name = Syn.SimpleIdent "StudentID"
  , Syn.definition = Syn.FieldDef
                     {
                       Syn.fieldType = Syn.DTypeTinyInt (Just 0)
                     , Syn.nullable = False
                     , Syn.autoIncrement = False
                     , Syn.uniqueKey = False
                     , Syn.primaryKey = False
                     }
  , Syn.colDefRefDef = Nothing
  }

ts8 :: TestTree
ts8 = testCase "ColumnDef2" $
  (parse createDef ""
    (Lex.alexScanTokens $ "CourseID tinyint not null"))
  @?= Right Syn.ColumnDef
  {
    Syn.name = Syn.SimpleIdent "CourseID"
  , Syn.definition = Syn.FieldDef
                     {
                       Syn.fieldType = Syn.DTypeTinyInt (Just 0)
                     , Syn.nullable = False
                     , Syn.autoIncrement = False
                     , Syn.uniqueKey = False
                     , Syn.primaryKey = False
                     }
  , Syn.colDefRefDef = Nothing
  }

ts9 :: TestTree
ts9 = testCase "ColumnDef3" $
  (parse createDef ""
    (Lex.alexScanTokens $ "Points tinyint not null"))
  @?= Right Syn.ColumnDef
  {
    Syn.name = Syn.SimpleIdent "Points"
  , Syn.definition = Syn.FieldDef
                     {
                       Syn.fieldType = Syn.DTypeTinyInt (Just 0)
                     , Syn.nullable = False
                     , Syn.autoIncrement = False
                     , Syn.uniqueKey = False
                     , Syn.primaryKey = False
                     }
  , Syn.colDefRefDef = Nothing
  }

ts10 :: TestTree
ts10 = testCase "PK1" $
  (parse createDef ""
    (Lex.alexScanTokens $ "PRIMARY KEY (StudentID, CourseID)"))
  @?= Right Syn.PKDef
  {
    Syn.pkColNames = [Syn.SimpleIdent "StudentID", Syn.SimpleIdent "CourseID"]
  }

ts11 :: TestTree
ts11 = testCase "Create Table2" $
  (parse createTableStmt ""
    (Lex.alexScanTokens $ "CREATE TABLE Scores \
                          \(StudentID tinyint not null, \
                          \ CourseID tinyint not null, \
                          \ Points tinyint not null, \
                          \ PRIMARY KEY (StudentID, CourseID), \
                          \ CHECK(NOT(1 <= CourseID and CourseID <= 10) or Points < 6))"))
  @?= Right Syn.CreateTable
  {
    Syn.isTemporary = False
  , Syn.tblName = Syn.SimpleIdent "Scores"
  , Syn.createDefinitions = [
      Syn.ColumnDef
      {
        Syn.name = Syn.SimpleIdent "StudentID"
      , Syn.definition = Syn.FieldDef
                         {
                           Syn.fieldType = Syn.DTypeTinyInt (Just 0)
                         , Syn.nullable = False
                         , Syn.autoIncrement = False
                         , Syn.uniqueKey = False
                         , Syn.primaryKey = False
                         }
      , Syn.colDefRefDef = Nothing
      },
      Syn.ColumnDef
      {
        Syn.name = Syn.SimpleIdent "CourseID"
      , Syn.definition = Syn.FieldDef
                         {
                           Syn.fieldType = Syn.DTypeTinyInt (Just 0)
                         , Syn.nullable = False
                         , Syn.autoIncrement = False
                         , Syn.uniqueKey = False
                         , Syn.primaryKey = False
                         }
      , Syn.colDefRefDef = Nothing
      },
      Syn.ColumnDef
      {
        Syn.name = Syn.SimpleIdent "Points"
      , Syn.definition = Syn.FieldDef
                         {
                           Syn.fieldType = Syn.DTypeTinyInt (Just 0)
                         , Syn.nullable = False
                         , Syn.autoIncrement = False
                         , Syn.uniqueKey = False
                         , Syn.primaryKey = False
                         }
      , Syn.colDefRefDef = Nothing
      },
      Syn.PKDef
      {
        Syn.pkColNames = [Syn.SimpleIdent "StudentID", Syn.SimpleIdent "CourseID"]
      },
      Syn.CheckExpr {
          Syn.checkExpr =
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
                                  (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident (Syn.SimpleIdent "CourseID"))))))
                              (Syn.BooleanPrimary
                                (Syn.BPLTE
                                  (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Ident (Syn.SimpleIdent "CourseID")))))
                                  (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "10"))))))
                            ]))))))
                (Syn.BooleanPrimary
                  (Syn.BPLT
                    (Syn.Predicate
                      (Syn.BitExpr
                        (Syn.SimpleExpr
                          (Syn.Ident (Syn.SimpleIdent "Points")))))
                    (Syn.BitExpr
                      (Syn.SimpleExpr
                        (Syn.Lit
                          (Syn.NLit "6")))))))
          }
      ]
  }

ts12 :: TestTree
ts12 = testCase "Select1" $
  (parse parseSelect ""
    (Lex.alexScanTokens $ "SELECT StudentName, Points FROM Students"))
  @?= Right (Syn.Select
  {
    Syn.selectAll = False
  , Syn.selectDistinct = False
  , Syn.selectExprs = [
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident (Syn.SimpleIdent "StudentName")))))),
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident (Syn.SimpleIdent "Points"))))))
      ]
  , Syn.selectTabRefs =
      Just (Syn.TableReferences
            {
              Syn.tableReferences = [
                Syn.TableReference
                {
                  Syn.tableFactor = Syn.TableFactor
                                    {
                                      Syn.tableFactorName = Syn.SimpleIdent "Students"
                                    }
                , Syn.joinTables = []
                }
                ]
            })
  , Syn.selectWhereCond = Nothing
  })

ts13 :: TestTree
ts13 = testCase "Select2" $
  (parse parseSelect ""
    (Lex.alexScanTokens $ "SELECT StudentName, Points FROM Students JOIN Scores"))
  @?= Right (Syn.Select
  {
    Syn.selectAll = False
  , Syn.selectDistinct = False
  , Syn.selectExprs = [
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident (Syn.SimpleIdent "StudentName")))))),
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident (Syn.SimpleIdent "Points"))))))
      ]
  , Syn.selectTabRefs =
      Just (Syn.TableReferences
            {
              Syn.tableReferences = [
                Syn.TableReference
                {
                  Syn.tableFactor = Syn.TableFactor
                                    {
                                      Syn.tableFactorName = Syn.SimpleIdent "Students"
                                    }
                , Syn.joinTables = [
                    Syn.InnerJoin
                    {
                      Syn.innerTableFactor = Syn.TableFactor
                                             {
                                               Syn.tableFactorName = Syn.SimpleIdent "Scores"
                                             }
                    , Syn.innerJoinConds = Nothing
                    }
                    ]
                }
                ]
            })
  , Syn.selectWhereCond = Nothing
  })

ts14 :: TestTree
ts14 = testCase "Select3" $
  (parse parseSelect ""
    (Lex.alexScanTokens $ "SELECT StudentName, Points \
                          \ FROM Students JOIN Scores ON Scores.StudentID = Students.StudentNr \
                          \ WHERE Scores.CourseID = 10 AND Scores.Points > 0"))
  @?= Right (Syn.Select
  {
    Syn.selectAll = False
  , Syn.selectDistinct = False
  , Syn.selectExprs = [
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident (Syn.SimpleIdent "StudentName")))))),
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident (Syn.SimpleIdent "Points"))))))
      ]
  , Syn.selectTabRefs =
      Just (Syn.TableReferences
           {
             Syn.tableReferences = [
               Syn.TableReference
               {
                 Syn.tableFactor = Syn.TableFactor
                                   {
                                     Syn.tableFactorName = Syn.SimpleIdent "Students"
                                   }
               , Syn.joinTables = [
                   Syn.InnerJoin
                   {
                     Syn.innerTableFactor = Syn.TableFactor
                                            {
                                              Syn.tableFactorName = Syn.SimpleIdent "Scores"
                                            }
                   , Syn.innerJoinConds = Just (Syn.JoinExpr
                                                {
                                                  Syn.joinExpr = Syn.BooleanPrimary
                                                             (Syn.BPEq
                                                               (Syn.Predicate
                                                                 (Syn.BitExpr
                                                                   (Syn.SimpleExpr
                                                                     (Syn.Ident (Syn.QualifiedIdent "Scores" "StudentID")))))
                                                               (Syn.BitExpr
                                                                 (Syn.SimpleExpr
                                                                   (Syn.Ident (Syn.QualifiedIdent "Students" "StudentNr")))))
                                                })
                   }
                   ]
               }
               ]
           })
  , Syn.selectWhereCond = Just (Syn.EAnd
                                 (Syn.BooleanPrimary
                                   (Syn.BPEq
                                     (Syn.Predicate
                                       (Syn.BitExpr
                                         (Syn.SimpleExpr
                                           (Syn.Ident
                                             (Syn.QualifiedIdent "Scores" "CourseID")))))
                                     (Syn.BitExpr
                                       (Syn.SimpleExpr
                                         (Syn.Lit
                                           (Syn.NLit "10"))))))
                                 (Syn.BooleanPrimary
                                   (Syn.BPGT
                                     (Syn.Predicate
                                       (Syn.BitExpr
                                         (Syn.SimpleExpr
                                           (Syn.Ident
                                             (Syn.QualifiedIdent "Scores" "Points")))))
                                     (Syn.BitExpr
                                       (Syn.SimpleExpr
                                         (Syn.Lit (Syn.NLit "0")))))))
  })

ts15 :: TestTree
ts15 = testCase "Select4" $
  (parse parseSelect ""
    (Lex.alexScanTokens $ "SELECT * FROM Students"))
  @?= Right (Syn.Select
  {
    Syn.selectAll = False
  , Syn.selectDistinct = False
  , Syn.selectExprs = [
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident (Syn.SimpleIdent "*"))))))
      ]
  , Syn.selectTabRefs =
      Just (Syn.TableReferences
            {
              Syn.tableReferences = [
                Syn.TableReference
                {
                  Syn.tableFactor = Syn.TableFactor
                                    {
                                      Syn.tableFactorName = Syn.SimpleIdent "Students"
                                    }
                , Syn.joinTables = []
                }
                ]
            })
  , Syn.selectWhereCond = Nothing
  })

ts16 :: TestTree
ts16 = testCase "Select5" $
  (parse parseSelect ""
    (Lex.alexScanTokens $ "SELECT Students.* FROM Students"))
  @?= Right (Syn.Select
  {
    Syn.selectAll = False
  , Syn.selectDistinct = False
  , Syn.selectExprs = [
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident (Syn.QualifiedIdent "Students" "*"))))))
      ]
  , Syn.selectTabRefs =
      Just (Syn.TableReferences
            {
              Syn.tableReferences = [
                Syn.TableReference
                {
                  Syn.tableFactor = Syn.TableFactor
                                    {
                                      Syn.tableFactorName = Syn.SimpleIdent "Students"
                                    }
                , Syn.joinTables = []
                }
                ]
            })
  , Syn.selectWhereCond = Nothing
  })

ts17 :: TestTree
ts17 = testCase "Create Table3" $
  (parse createTableStmt ""
   (Lex.alexScanTokens $ "CREATE TABLE School.Students \
                         \ (StudentNr tinyint not null PRIMARY KEY, \
                         \ StudentName char(100) not null)"))
  @?= Right Syn.CreateTable
  {
    Syn.isTemporary = False
  , Syn.tblName = Syn.QualifiedIdent "School" "Students"
  , Syn.createDefinitions = [
      Syn.ColumnDef { Syn.name = Syn.SimpleIdent "StudentNr"
                    , Syn.definition =
                        Syn.FieldDef { Syn.fieldType = Syn.DTypeTinyInt (Just 0)
                                     , Syn.nullable = False
                                     , Syn.autoIncrement = False
                                     , Syn.uniqueKey = False
                                     , Syn.primaryKey = True
                                     }
                    , Syn.colDefRefDef = Nothing
                    },
        Syn.ColumnDef { Syn.name = Syn.SimpleIdent "StudentName"
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

ts18 :: TestTree
ts18 = testCase "Symbolic Expression1" $
  (parse parseExpr ""
   (Lex.alexScanTokens $ "1 <= @symbolic1@ and @symbolic1@ <= 10"))
  @?= Right (Syn.EAnd
  (Syn.BooleanPrimary
   (Syn.BPLTE
    (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "1")))))
    (Syn.BitExpr (Syn.SimpleExpr (Syn.SymbolicE 1)))))
  (Syn.BooleanPrimary
   (Syn.BPLTE
    (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.SymbolicE 1))))
    (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "10")))))))

ts19 :: TestTree
ts19 = testCase "Symbolic Expression2" $
  (parse parseExpr ""
   (Lex.alexScanTokens $ "NOT(1 <= @symbolic2@ and @symbolic2@ <= 10)"))
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
                (Syn.BitExpr (Syn.SimpleExpr (Syn.SymbolicE 2)))))
            (Syn.BooleanPrimary
              (Syn.BPLTE
                (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.SymbolicE 2))))
                (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "10"))))))
          ]))))))

ts20 :: TestTree
ts20 = testCase "Symbolic Expression3" $
  (parse parseExpr ""
    (Lex.alexScanTokens $ "NOT(1 <= @symbolic1@ and @symbolic1@ <= 10) or @symbolic2@ < 6"))
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
                      (Syn.BitExpr (Syn.SimpleExpr (Syn.SymbolicE 1)))))
                  (Syn.BooleanPrimary
                    (Syn.BPLTE
                      (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.SymbolicE 1))))
                      (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "10"))))))
                ]))))))
    (Syn.BooleanPrimary
      (Syn.BPLT
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.SymbolicE 2))))
        (Syn.BitExpr
          (Syn.SimpleExpr
            (Syn.Lit
              (Syn.NLit "6")))))))

ts21 :: TestTree
ts21 = testCase "Symbolic Expression4" $
  (parse parseExpr ""
    (Lex.alexScanTokens $ "1 or @symbolic3@ < 6"))
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
              (Syn.SymbolicE 3))))
        (Syn.BitExpr
          (Syn.SimpleExpr
            (Syn.Lit
              (Syn.NLit "6")))))))

ts22 :: TestTree
ts22 = testCase "Symbolic Select1" $
  (parse parseSelect ""
    (Lex.alexScanTokens $ "SELECT StudentName, Points \
                          \ FROM Students JOIN Scores ON Scores.StudentID = Students.StudentNr \
                          \ WHERE Scores.CourseID = @symbolic1@ AND Scores.Points > @symbolic2@"))
  @?= Right (Syn.Select
  {
    Syn.selectAll = False
  , Syn.selectDistinct = False
  , Syn.selectExprs = [
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident (Syn.SimpleIdent "StudentName")))))),
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Ident (Syn.SimpleIdent "Points"))))))
      ]
  , Syn.selectTabRefs =
      Just (Syn.TableReferences
           {
             Syn.tableReferences = [
               Syn.TableReference
               {
                 Syn.tableFactor = Syn.TableFactor
                                   {
                                     Syn.tableFactorName = Syn.SimpleIdent "Students"
                                   }
               , Syn.joinTables = [
                   Syn.InnerJoin
                   {
                     Syn.innerTableFactor = Syn.TableFactor
                                            {
                                              Syn.tableFactorName = Syn.SimpleIdent "Scores"
                                            }
                   , Syn.innerJoinConds = Just (Syn.JoinExpr
                                                {
                                                  Syn.joinExpr = Syn.BooleanPrimary
                                                             (Syn.BPEq
                                                               (Syn.Predicate
                                                                 (Syn.BitExpr
                                                                   (Syn.SimpleExpr
                                                                     (Syn.Ident (Syn.QualifiedIdent "Scores" "StudentID")))))
                                                               (Syn.BitExpr
                                                                 (Syn.SimpleExpr
                                                                   (Syn.Ident (Syn.QualifiedIdent "Students" "StudentNr")))))
                                                })
                   }
                   ]
               }
               ]
           })
  , Syn.selectWhereCond = Just (Syn.EAnd
                                 (Syn.BooleanPrimary
                                   (Syn.BPEq
                                     (Syn.Predicate
                                       (Syn.BitExpr
                                         (Syn.SimpleExpr
                                           (Syn.Ident
                                             (Syn.QualifiedIdent "Scores" "CourseID")))))
                                     (Syn.BitExpr
                                       (Syn.SimpleExpr (Syn.SymbolicE 1)))))
                                 (Syn.BooleanPrimary
                                   (Syn.BPGT
                                     (Syn.Predicate
                                       (Syn.BitExpr
                                         (Syn.SimpleExpr
                                           (Syn.Ident
                                             (Syn.QualifiedIdent "Scores" "Points")))))
                                     (Syn.BitExpr
                                       (Syn.SimpleExpr (Syn.SymbolicE 2))))))
  })

ts23 :: TestTree
ts23 = testCase "Symbolic Expression5" $
  (parse parseExpr ""
   (Lex.alexScanTokens $ "1 <= '@symbolic1@' and \"@symbolic1@\" <= 10"))
  @?= Right (Syn.EAnd
  (Syn.BooleanPrimary
   (Syn.BPLTE
    (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "1")))))
    (Syn.BitExpr (Syn.SimpleExpr (Syn.SymbolicE 1)))))
  (Syn.BooleanPrimary
   (Syn.BPLTE
    (Syn.Predicate (Syn.BitExpr (Syn.SimpleExpr (Syn.SymbolicE 1))))
    (Syn.BitExpr (Syn.SimpleExpr (Syn.Lit (Syn.NLit "10")))))))

ts24 :: TestTree
ts24 = testCase "Delete1" $
  (parse parseDelete ""
   (Lex.alexScanTokens $ "DELETE FROM Students"))
  @?= Right (Syn.Delete
  {
    Syn.deleteTblName = Syn.SimpleIdent "Students"
  , Syn.deleteWhereCond = Nothing
  })

ts25 :: TestTree
ts25 = testCase "Delete2" $
  (parse parseDelete ""
   (Lex.alexScanTokens $ "DELETE FROM Students WHERE Students.StudentNr > 7"))
  @?= Right (Syn.Delete
  {
    Syn.deleteTblName = Syn.SimpleIdent "Students"
  , Syn.deleteWhereCond = Just (Syn.BooleanPrimary
                                (Syn.BPGT
                                 (Syn.Predicate
                                  (Syn.BitExpr
                                   (Syn.SimpleExpr
                                    (Syn.Ident
                                     (Syn.QualifiedIdent "Students" "StudentNr")))))
                                 (Syn.BitExpr
                                  (Syn.SimpleExpr
                                   (Syn.Lit
                                    (Syn.NLit "7"))))))
  })

ts26 :: TestTree
ts26 = testCase "Symbolic Delete1" $
  (parse parseDelete ""
   (Lex.alexScanTokens $ "DELETE FROM Students WHERE Students.StudentNr > @symbolic1@"))
  @?= Right (Syn.Delete
  {
    Syn.deleteTblName = Syn.SimpleIdent "Students"
  , Syn.deleteWhereCond = Just (Syn.BooleanPrimary
                                (Syn.BPGT
                                 (Syn.Predicate
                                  (Syn.BitExpr
                                   (Syn.SimpleExpr
                                    (Syn.Ident
                                     (Syn.QualifiedIdent "Students" "StudentNr")))))
                                 (Syn.BitExpr
                                  (Syn.SimpleExpr
                                   (Syn.SymbolicE 1)))))
  })  

ts27 :: TestTree
ts27 = testCase "Insert1" $
  (parse parseInsert ""
   (Lex.alexScanTokens $ "INSERT INTO Students VALUES (1, 'abc')"))
  @?= Right (Syn.Insert
  {
    Syn.insertTblName = Syn.SimpleIdent "Students"
  , Syn.insertColNames = Nothing
  , Syn.insertValues = [
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Lit
                (Syn.NLit "1")))))),
        (Syn.BooleanPrimary
          (Syn.Predicate
            (Syn.BitExpr
              (Syn.SimpleExpr
                (Syn.Lit
                  (Syn.SLit "abc"))))))
      ]
  })

ts28 :: TestTree
ts28 = testCase "Insert2" $
  (parse parseInsert ""
   (Lex.alexScanTokens $ "INSERT INTO Students (StudentNr, StudentName) VALUES (1, 'abc')"))
  @?= Right (Syn.Insert
  {
    Syn.insertTblName = Syn.SimpleIdent "Students"
  , Syn.insertColNames = Just [
      Syn.SimpleIdent "StudentNr",
      Syn.SimpleIdent "StudentName"
      ]
  , Syn.insertValues = [
      (Syn.BooleanPrimary
        (Syn.Predicate
          (Syn.BitExpr
            (Syn.SimpleExpr
              (Syn.Lit
                (Syn.NLit "1")))))),
        (Syn.BooleanPrimary
          (Syn.Predicate
            (Syn.BitExpr
              (Syn.SimpleExpr
                (Syn.Lit
                  (Syn.SLit "abc"))))))
      ]
  })

ts29 :: TestTree
ts29 = testCase "Update1" $
  (parse parseUpdate ""
   (Lex.alexScanTokens $ "UPDATE Students SET StudentNr=17"))
  @?= Right (Syn.Update
  {
    Syn.updateTblRef = Syn.TableReference
                       {
                         Syn.tableFactor = Syn.TableFactor
                                           {
                                             Syn.tableFactorName = Syn.SimpleIdent "Students"
                                           }
                       , Syn.joinTables = []
                       }
  , Syn.updateColNameValues = [
      ( Syn.SimpleIdent "StudentNr"
      , (Syn.BooleanPrimary
         (Syn.Predicate
          (Syn.BitExpr
           (Syn.SimpleExpr
            (Syn.Lit
             (Syn.NLit "17")))))))
      ]
  , Syn.updateWhereCond = Nothing
  })

ts30 :: TestTree
ts30 = testCase "Failure Case 1" $
  (parse parseSelect ""
   (Lex.alexScanTokens $ "select info.*, employees.*, punchlist.* \
              \ from info, employees, punchlist \
              \ where info.timestamp = employees.tstamp and info.fullname = employees.empfullname \
              \ and info.`inout` = punchlist.punchitems and employees.disabled <> '1' \
              \ and employees.empfullname <> 'admin' \
              \ order by `fullname`"))
  @?= Left (newErrorUnknown (newPos "" 0 0))
