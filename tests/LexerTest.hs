module LexerTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified MySQL.Lexer      as Lex
import qualified MySQL.Token      as Tok


testCases :: [TestTree]
testCases = [ts1, ts2, ts3, ts4]

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
