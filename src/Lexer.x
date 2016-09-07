{

module Lexer where

import Data.Char (toLower)
import Token

}

%wrapper "basic"

$space = [ \ \t ]

$letter = [a-zA-Z_]
$identletter = [a-zA-Z_0-9]

$digit = [0-9]
$hexdigit = [0-9a-fA-F]

$dqstr = \0-\255 # [ \" \n \\ ]
$sqstr = \0-\255 # [ \' \n \\ ]
$longstr = \0-\255

@charescd = \\ ([ntvbrfa\\'"] | $digit{1,3} | x$hexdigit{2} | \n)
@charescs = \\ ([ntvbrfa\\"'] | $digit{1,3} | x$hexdigit{2} | \n)

@digits = $digit+
@hexdigits = $hexdigit+


tokens :-

       <0> $white+			;

       -- Identifiers:
       -- An identifier maybe quoted ("`") or unquoted. If an identifier contains special characters
       -- or is a reserved word, you must quote it whenever you refer to it.
       -- (Exception: a reserved word that follows a period in a qualified name need not be quoted).
       --
       -- Allowed characters
       -- ASCII: [0-9,a-z,A-Z$_] (ASCII NUL (\0) is not allowed)
       --
       -- Note
       -- * May begin with a digit but unless quoted, may not consist sorely of digits.
       --
       <0> \` $letter $identletter* \`	{ \s -> LTokIdent s }
       <0> $letter $identletter*	{ ident }

       <0> @digits 		 	{ \s -> LTokNum s }

       <0> \"($dqstr|@charescd)*\"    	{ \s -> LTokStr s }
       <0> \'($sqstr|@charescs)*\'	{ \s -> LTokStr s }

       <0> "!"				{ \s -> LTokNot }
       <0> "-"				{ \s -> LTokUMinus }
       <0> "~"				{ \s -> LTokUInv }
       <0> "^"				{ \s -> LTokXOr }
       <0> "*"				{ \s -> LTokMul }
       <0> "/"				{ \s -> LTokDiv }
       <0> "%"				{ \s -> LTokMod }
       <0> "+"				{ \s -> LTokAdd }
       <0> "<<"				{ \s -> LTokLShift }
       <0> ">>"				{ \s -> LTokRShift }
       <0> "&"				{ \s -> LTokBitAnd }
       <0> "|"				{ \s -> LTokBitOr }
       <0> "="				{ \s -> LTokEq }
       <0> "<=>"			{ \s -> LTokSafeNotEq }
       <0> ">="				{ \s -> LTokGTE }
       <0> ">"				{ \s -> LTokGT }
       <0> "<="				{ \s -> LTokLTE }
       <0> "<"				{ \s -> LTokLT }
       <0> "<>"				{ \s -> LTokNotEq }
       <0> "!="				{ \s -> LTokNotEq }
       <0> "&&"				{ \s -> LTokAnd }
       <0> "||"				{ \s -> LTokOr }

{

ident :: String -> LToken
ident s = case (fmap toLower s) of
       "add" -> LTokAdd
       "all" -> LTokAll
       "alter" -> LTokAlter
       "and" -> LTokAnd
       "as" -> LTokAs
       "asc" -> LTokAsc
       "before" -> LTokBefore
       "between" -> LTokBetween
       "bigint" -> LTokBigInt
       "binary" -> LTokBinary
       "blob" -> LTokBlob
       "both" -> LTokBoth
       "by" -> LTokBy
       "case" -> LTokCase
       "char" -> LTokChar
       "character" -> LTokCharacter
       "collate" -> LTokCollate
       "column" -> LTokColumn
       "constraint" -> LTokConstraint
       "create" -> LTokCreate
       "cross" -> LTokCross
       "decimal" -> LTokDecimal
       "declare" -> LTokDeclare
       "default" -> LTokDefault
       "delete" -> LTokDelete
       "desc" -> LTokDesc
       "distinct" -> LTokDistinct
       "div" -> LTokDiv
       "double" -> LTokDouble
       "drop" -> LTokDrop
       "exists" -> LTokExists
       "false" -> LTokFalse
       "float" -> LTokFloat
       "foreign" -> LTokForeign
       "from" -> LTokFrom
       "fulltext" -> LTokFullText
       "group" -> LTokGroup
       "having" -> LTokHaving
       "ignore" -> LTokIgnore
       "in" -> LTokIn
       "index" -> LTokIndex
       "inner" -> LTokInner
       "insert" -> LTokInsert
       "int" -> LTokInt
       "integer" -> LTokInteger
       "interval" -> LTokInterval
       "is" -> LTokIs
       "join" -> LTokJoin
       "key" -> LTokKey
       "keys" -> LTokKeys
       "left" -> LTokLeft
       "like" -> LTokLike
       "limit" -> LTokLimit
       "long" -> LTokLong
       "match" -> LTokMatch
       "mediumint" -> LTokMediumInt
       "mod" -> LTokMod
       "not" -> LTokNot
       "null" -> LTokNull
       "on" -> LTokOn
       "or" -> LTokOr
       "order" -> LTokOrder
       "outer" -> LTokOuter
       "primary" -> LTokPrimary
       "range" -> LTokRange
       "real" -> LTokReal
       "references" -> LTokReferences
       "replace" -> LTokReplace
       "schema" -> LTokSchema
       "select" -> LTokSelect
       "smallint" -> LTokSmallInt
       "table" -> LTokTable
       "tinyint" -> LTokTinyInt
       "true" -> LTokTrue
       "union" -> LTokUnion
       "unique" -> LTokUnique
       "unsigned" -> LTokUnsigned
       "update" -> LTokUpdate
       "using" -> LTokUsing
       "values" -> LTokValues
       "varchar" -> LTokVarChar
       "varying" -> LTokVarying
       "where" -> LTokWhere
       "with" -> LTokWith
       ident' -> LTokIdent ident'
}
