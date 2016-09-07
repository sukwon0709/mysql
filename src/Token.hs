module Token where

-- | MySQL tokens
data LToken = LTokAdd           -- ADD (R)
            | LTokAll           -- ALL (R)
            | LTokAlter         -- ALTER (R)
            | LTokAnd           -- AND (R)
            | LTokAs            -- AS (R)
            | LTokAsc           -- ASC (R)
            | LTokBefore        -- BEFORE (R)
            | LTokBetween       -- BETWEEN (R)
            | LTokBigInt        -- BIGINT (R)
            | LTokBinary        -- BINARY (R)
            | LTokBlob          -- BLOB (R)
            | LTokBoth          -- BOTH (R)
            | LTokBy            -- BY (R)
            | LTokCase          -- CASE (R)
            | LTokChar          -- CHAR (R)
            | LTokCharacter     -- CHARACTER (R)
            | LTokCollate       -- COLLATE (R)
            | LTokColumn        -- COLUMN (R)
            | LTokConstraint    -- CONSTRAINT (R)
            | LTokCreate        -- CREATE (R)
            | LTokCross         -- CROSS (R)
            | LTokDecimal       -- DECIMAL (R)
            | LTokDeclare       -- DECLARE (R)
            | LTokDefault       -- DEFAULT (R)
            | LTokDelete        -- DELETE (R)
            | LTokDesc          -- DESC (R)
            | LTokDistinct      -- DISTINCT (R)
            | LTokDiv           -- DIV (R)
            | LTokDouble        -- DOUBLE (R)
            | LTokDrop          -- DROP (R)
            | LTokExists        -- EXISTS (R)
            | LTokFalse         -- FALSE (R)
            | LTokFloat         -- FLOAT (R)
            | LTokForeign       -- FOREIGN (R)
            | LTokFrom          -- FROM (R)
            | LTokFullText      -- FULLTEXT (R)
            | LTokGroup         -- GROUP (R)
            | LTokHaving        -- HAVING (R)
            | LTokIgnore        -- IGNORE (R)
            | LTokIn            -- IN (R)
            | LTokIndex         -- INDEX (R)
            | LTokInner         -- INNER (R)
            | LTokInsert        -- INSERT (R)
            | LTokInt           -- INT (R)
            | LTokInteger       -- INTEGER (R)
            | LTokInterval      -- INTERVAL (R)
            | LTokIs            -- IS (R)
            | LTokJoin          -- JOIN (R)
            | LTokKey           -- KEY (R)
            | LTokKeys          -- KEYS (R)
            | LTokLeft          -- LEFT (R)
            | LTokLike          -- LIKE (R)
            | LTokLimit         -- LIMIT (R)
            | LTokLong          -- LONG (R)
            | LTokMatch         -- MATCH (R)
            | LTokMediumInt     -- MEDIUMINT (R)
            | LTokMod           -- MOD (R)
            | LTokNot           -- NOT (R)
            | LTokNull          -- NULL (R)
            | LTokOn            -- ON (R)
            | LTokOr            -- OR (R)
            | LTokOrder         -- ORDER (R)
            | LTokOuter         -- OUTER (R)
            | LTokPrimary       -- PRIMARY (R)
            | LTokRange         -- RANGE (R)
            | LTokReal          -- REAL (R)
            | LTokReferences    -- REFERENCES (R)
            | LTokReplace       -- REPLACE (R)
            | LTokSchema        -- SCHEMA (R)
            | LTokSelect        -- SELECT (R)
            | LTokSmallInt      -- SMALLINT (R)
            | LTokTable         -- TABLE (R)
            | LTokTinyInt       -- TINYINT (R)
            | LTokTrue          -- TRUE (R)
            | LTokUnion         -- UNION (R)
            | LTokUnique        -- UNIQUE (R)
            | LTokUnsigned      -- UNSIGNED (R)
            | LTokUpdate        -- UPDATE (R)
            | LTokUsing         -- USING (R)
            | LTokValues        -- VALUES (R)
            | LTokVarChar       -- VARCHAR (R)
            | LTokVarying       -- VARYING (R)
            | LTokWhere         -- WHERE (R)
            | LTokWith          -- WITH (R)

              -- operators
            | LTokUPlus         -- unary plus
            | LTokUMinus        -- unary minus
            | LTokUInv          -- unary bit inversion
            | LTokUNot          -- unary not
            | LTokXOr           -- xor
            | LTokMul           -- muliply
            | LTokSub           -- subtract
            | LTokLShift        -- left shift
            | LTokRShift        -- right shift
            | LTokBitAnd        -- bitwise and
            | LTokBitOr         -- bitwise or
            | LTokEq            -- equal
            | LTokSafeNotEq     -- null safe not equal
            | LTokGTE           -- >=
            | LTokGT            -- >
            | LTokLTE           -- <=
            | LTokLT            -- <
            | LTokNotEq         -- <>, !=

              -- terms with values
            | LTokNum String    -- number constant
            | LTokStr String    -- string constant
            | LTokIdent String  -- identifier
            | LTokEof           -- end of file
            deriving Eq

instance Show LToken where
  show LTokAdd        = "ADD"
  show LTokAll        = "ALL"
  show LTokAlter      = "ALTER"
  show LTokAnd        = "AND"
  show LTokAs         = "AS"
  show LTokAsc        = "ASC"
  show LTokBefore     = "BEFORE"
  show LTokBetween    = "BETWEEN"
  show LTokBigInt     = "BIGINT"
  show LTokBinary     = "BINARY"
  show LTokBlob       = "BLOB"
  show LTokBoth       = "BOTH"
  show LTokBy         = "BY"
  show LTokCase       = "CASE"
  show LTokChar       = "CHAR"
  show LTokCharacter  = "CHARACTER"
  show LTokCollate    = "COLLATE"
  show LTokColumn     = "COLUMN"
  show LTokConstraint = "CONSTRAINT"
  show LTokCreate     = "CREATE"
  show LTokCross      = "CROSS"
  show LTokDecimal    = "DECIMAL"
  show LTokDeclare    = "DECLARE"
  show LTokDefault    = "DEFAULT"
  show LTokDelete     = "DELETE"
  show LTokDesc       = "DESC"
  show LTokDistinct   = "DISTINCT"
  show LTokDiv        = "DIV"
  show LTokDouble     = "DOUBLE"
  show LTokDrop       = "DROP"
  show LTokExists     = "EXISTS"
  show LTokFalse      = "FALSE"
  show LTokFloat      = "FLOAT"
  show LTokForeign    = "FOREIGN"
  show LTokFrom       = "FROM"
  show LTokFullText   = "FULLTEXT"
  show LTokGroup      = "GROUP"
  show LTokHaving     = "HAVING"
  show LTokIgnore     = "IGNORE"
  show LTokIn         = "IN"
  show LTokIndex      = "INDEX"
  show LTokInner      = "INNER"
  show LTokInsert     = "INSERT"
  show LTokInt        = "INT"
  show LTokInteger    = "INTEGER"
  show LTokInterval   = "INTERVAL"
  show LTokIs         = "IS"
  show LTokJoin       = "JOIN"
  show LTokKey        = "KEY"
  show LTokKeys       = "KEYS"
  show LTokLeft       = "LEFT"
  show LTokLike       = "LIKE"
  show LTokLimit      = "LIMIT"
  show LTokLong       = "LONG"
  show LTokMatch      = "MATCH"
  show LTokMediumInt  = "MEDIUMINT"
  show LTokMod        = "MOD"
  show LTokNot        = "NOT"
  show LTokNull       = "NULL"
  show LTokOn         = "ON"
  show LTokOr         = "OR"
  show LTokOrder      = "ORDER"
  show LTokOuter      = "OUTER"
  show LTokPrimary    = "PRIMARY"
  show LTokRange      = "RANGE"
  show LTokReal       = "REAL"
  show LTokReferences = "REFERENCES"
  show LTokReplace    = "REPLACE"
  show LTokSchema     = "SCHEMA"
  show LTokSelect     = "SELECT"
  show LTokSmallInt   = "SMALLINT"
  show LTokTable      = "TABLE"
  show LTokTinyInt    = "TINYINT"
  show LTokTrue       = "TRUE"
  show LTokUnion      = "UNION"
  show LTokUnique     = "UNIQUE"
  show LTokUnsigned   = "UNSIGNED"
  show LTokUpdate     = "UPDATE"
  show LTokUsing      = "USING"
  show LTokValues     = "VALUES"
  show LTokVarChar    = "VARCHAR"
  show LTokVarying    = "VARYING"
  show LTokWhere      = "WHERE"
  show LTokWith       = "WITH"

  show LTokUPlus      = "+"
  show LTokUMinus     = "-"
  show LTokUInv       = "~"
  show LTokUNot       = "!"
  show LTokXOr        = "^"
  show LTokMul        = "*"
  show LTokSub        = "-"
  show LTokLShift     = "<<"
  show LTokRShift     = ">>"
  show LTokBitAnd     = "&"
  show LTokBitOr      = "|"
  show LTokEq         = "="
  show LTokSafeNotEq  = "<=>"
  show LTokGTE        = ">="
  show LTokGT         = ">"
  show LTokLTE        = "<="
  show LTokLT         = "<"
  show LTokNotEq      = "<>"

  show (LTokNum n)    = "number: " ++ n
  show (LTokStr s)    = "string: " ++ s
  show (LTokIdent i)  = "identifier: " ++ i
  show LTokEof        = "EOF"