module MySQL.Parser
  ( parseMySQL
  , module MySQL.Token
  , module MySQL.Syntax
  ) where

import           MySQL.Lexer
import qualified MySQL.ParserInternal   as Par
import           MySQL.Syntax
import           MySQL.Token
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim


parseMySQL :: String -> Either ParseError Statement
parseMySQL s = parse parseMySQL' "" (alexScanTokens s)

parseMySQL' :: Par.Parser Statement
parseMySQL' = try (CreateTableStmt <$> Par.createTableStmt)
              <|> try (SelectStmt <$> Par.parseSelect)
              <|> try (InsertStmt <$> Par.parseInsert)
              <|> try (UpdateStmt <$> Par.parseUpdate)
              <|> (DeleteStmt <$> Par.parseDelete)
