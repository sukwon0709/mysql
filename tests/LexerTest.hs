module LexerTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified MySQL.Lexer      as Lex
import qualified MySQL.Token      as Tok


testCases :: [TestTree]
testCases = [ts1, ts2, ts3, ts4, ts5]

ts1 :: TestTree
ts1 = testCase "Identifier test1" $
  Lex.alexScanTokens "sukwon" @?= [Tok.LTokIdent (Tok.LIdentSimpleToken "sukwon")]

ts2 :: TestTree
ts2 = testCase "Identifier test2" $
  Lex.alexScanTokens "sukwon oh" @?= [Tok.LTokIdent (Tok.LIdentSimpleToken "sukwon"), Tok.LTokIdent (Tok.LIdentSimpleToken "oh")]

ts3 :: TestTree
ts3 = testCase "Identifier test3" $
  Lex.alexScanTokens "select * from `table`" @?= [Tok.LTokSelect,
                                                  Tok.LTokIdent (Tok.LIdentSimpleToken "*"),
                                                  Tok.LTokFrom,
                                                  Tok.LTokIdent (Tok.LIdentSimpleToken "table")]

ts4 :: TestTree
ts4 = testCase "Symbolic test1" $
  Lex.alexScanTokens "@symbolic1@" @?= [Tok.LTokSymbolic 1]

ts5 :: TestTree
ts5 = testCase "Failure case 1" $
  Lex.alexScanTokens "select info.*, employees.*, punchlist.* \
              \ from info, employees, punchlist \
              \ where info.timestamp = employees.tstamp and info.fullname = employees.empfullname \
              \ and info.`inout` = punchlist.punchitems and employees.disabled <> '1' \
              \ and employees.empfullname <> 'admin' \
              \ order by `fullname`" @?= [ Tok.LTokSelect
                                         , Tok.LTokIdent (Tok.LIdentQualifiedToken "info" "*")
                                         , Tok.LTokComma
                                         , Tok.LTokIdent (Tok.LIdentQualifiedToken "employees" "*")
                                         , Tok.LTokComma
                                         , Tok.LTokIdent (Tok.LIdentQualifiedToken "punchlist" "*")
                                         , Tok.LTokFrom
                                         , Tok.LTokIdent (Tok.LIdentSimpleToken "info")
                                         , Tok.LTokComma
                                         , Tok.LTokIdent (Tok.LIdentSimpleToken "employees")
                                         , Tok.LTokComma
                                         , Tok.LTokIdent (Tok.LIdentSimpleToken "punchlist")
                                         , Tok.LTokWhere
                                         , Tok.LTokIdent (Tok.LIdentQualifiedToken "info" "timestamp")
                                         , Tok.LTokEq
                                         , Tok.LTokIdent (Tok.LIdentQualifiedToken "employees" "tstamp")
                                         , Tok.LTokAnd
                                         , Tok.LTokIdent (Tok.LIdentQualifiedToken "info" "fullname")
                                         , Tok.LTokEq
                                         , Tok.LTokIdent (Tok.LIdentQualifiedToken "employees" "empfullname")
                                         , Tok.LTokAnd
                                         , Tok.LTokIdent (Tok.LIdentQualifiedToken "info" "inout")
                                         , Tok.LTokEq
                                         , Tok.LTokIdent (Tok.LIdentQualifiedToken "punchlist" "punchitems")
                                         , Tok.LTokAnd
                                         , Tok.LTokIdent (Tok.LIdentQualifiedToken "employees" "disabled")
                                         , Tok.LTokNotEq
                                         , Tok.LTokStr "1"
                                         , Tok.LTokAnd
                                         , Tok.LTokIdent (Tok.LIdentQualifiedToken "employees" "empfullname")
                                         , Tok.LTokNotEq
                                         , Tok.LTokStr "admin"
                                         , Tok.LTokOrder
                                         , Tok.LTokBy
                                         , Tok.LTokIdent (Tok.LIdentSimpleToken "fullname")
                                         ]
