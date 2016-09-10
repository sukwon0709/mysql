module MySQL.Parser
  ( parseMySQL
  , module Token
  , module Syntax
  ) where

import           Lexer
import qualified Parser                 as Par
import           Syntax
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim
import           Token


parseMySQL :: String -> Either ParseError Statement
parseMySQL s = parse parseMySQL' "" (alexScanTokens s)

parseMySQL' :: Par.Parser Statement
parseMySQL' = try (CreateTableStmt <$> Par.createTableStmt)
              <|> (SelectStmt <$> Par.parseSelect)
