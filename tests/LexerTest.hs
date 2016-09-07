module LexerTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Lexer            as Lex
import qualified Token            as Tok


testCases :: [TestTree]
testCases = [ts1, ts2, ts3]

ts1 :: TestTree
ts1 = testCase "Identifier test1" $
  Lex.alexScanTokens "sukwon" @?= [Tok.LTokIdent "sukwon"]

ts2 :: TestTree
ts2 = testCase "Identifier test2" $
  Lex.alexScanTokens "sukwon oh" @?= [Tok.LTokIdent "sukwon", Tok.LTokIdent "oh"]

ts3 :: TestTree
ts3 = testCase "Identifier test3" $
  Lex.alexScanTokens "select * from `table`" @?= [Tok.LTokSelect,
                                                  Tok.LTokMul,
                                                  Tok.LTokFrom,
                                                  Tok.LTokIdent "`table`"]
