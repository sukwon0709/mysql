{

module MySQL.Lexer where

import Data.Char (toLower)
import Data.List.Split (splitOn)
import MySQL.Token

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
@idents = $letter $identletter*
@idents_star = @idents|"*"

@dbtblcol1 = @idents "." @idents "." @idents_star
@dbtblcol2 = @idents "." @idents "." \`@idents\`
@dbtblcol3 = @idents "." \`@idents\` "." @idents_star
@dbtblcol4 = \`@idents\` "." @idents "." @idents_star
@dbtblcol5 = @idents "." \`@idents\` "." \`@idents\`
@dbtblcol6 = \`@idents\` "." @idents "." \`@idents\`
@dbtblcol7 = \`@idents\` "." \`@idents\` "." @idents_star
@dbtblcol8 = \`@idents\` "." \`@idents\` "." \`@idents\`

@tblcol1 = @idents "." @idents_star
@tblcol2 = @idents "." \`@idents\`
@tblcol3 = \`@idents\` "." @idents_star
@tblcol4 = \`@idents\` "." \`@idents\`

tokens :-

       <0> $white+                      ;

       -- Symbolic terms:
       -- We allow symbolic expressions to be a part of SQL queries.
       -- Symbolic terms are serialized into bytestrings and translated into placeholder values,
       -- such as @symbolic1@, @symbolic2@, ...
       --
       
       <0> \'"@symbolic" @digits "@"\'  { \s -> let n = read . init . init . drop 10 $ s :: Integer
                                                in LTokSymbolic n
                                        }

       <0> \""@symbolic" @digits "@"\"  { \s -> let n = read . init . init . drop 10 $ s :: Integer
                                                in LTokSymbolic n
                                        }

       <0> "@symbolic" @digits "@"      { \s -> let n = read . init . drop 9 $ s :: Integer
                                                in LTokSymbolic n
                                        }

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
       <0> @dbtblcol1|@dbtblcol2|@dbtblcol3|@dbtblcol4|@dbtblcol5|@dbtblcol6|@dbtblcol7|@dbtblcol8
                                        { \s -> let [s1, s2, s3] = splitOn "." (filter (/= '`') s)
                                                in LTokIdent $ LIdentDoubleQualifiedToken s1 s2 s3
                                        }

       <0> @tblcol1|@tblcol2|@tblcol3|@tblcol4
					{ \s -> let [s1, s2] = splitOn "." (filter (/= '`') s)
                                                in LTokIdent $ LIdentQualifiedToken s1 s2
                                        }       

       <0> \`@idents_star\`             { \s -> LTokIdent $ LIdentSimpleToken (filter (/= '`') s) }
       <0> @idents_star                 { ident }

       <0> @digits                      { \s -> LTokNum s }

       <0> \"($dqstr|@charescd)*\"      { \s -> LTokStr (filter (/= '\"') s) }
       <0> \'($sqstr|@charescs)*\'      { \s -> LTokStr (filter (/= '\'') s) }

       -- Syntax
       --
       <0> "("                          { \s -> LTokOpenPar }           -- (
       <0> ")"                          { \s -> LTokClosePar }          -- )
       <0> ","                          { \s -> LTokComma }             -- ,
                    
       -- Operators
       --
       <0> "&"                          { \s -> LTokBitAnd }            -- Bitwise AND
       <0> "~"                          { \s -> LTokBitInv }            -- Bitwise inversion
       <0> "|"                          { \s -> LTokBitOr }             -- Bitwise OR
       <0> "^"                          { \s -> LTokBitXOr }            -- Bitwise XOR
       <0> "/"                          { \s -> LTokDiv }               -- division operator
       <0> "<<"                         { \s -> LTokLShift }            -- left shift
       <0> "-"                          { \s -> LTokMinus }             -- minus / negative operator
       <0> "%"                          { \s -> LTokMod }               -- modulo operator
       <0> "+"                          { \s -> LTokPlus }              -- addition operator
       <0> ">>"                         { \s -> LTokRShift }            -- right shift
       <0> "*"                          { \s -> LTokMul }               -- multipliciation operator

       -- Comparison Operators
       -- =, <=>, >, >=, <, <=, !=, <>
       <0> "="                          { \s -> LTokEq }                -- equal operator (could be assignment)
       <0> "<=>"                        { \s -> LTokSafeNotEq }         -- NULL-safe equal operator
       <0> ">"                          { \s -> LTokGT }                -- greater than operator
       <0> ">="                         { \s -> LTokGTE }               -- greater than or equal operator
       <0> "<"                          { \s -> LTokLT }                -- less than operator
       <0> "<="                         { \s -> LTokLTE }               -- less than or equal operator
       <0> "!="                         { \s -> LTokNotEq }             -- not equal operator
       <0> "<>"                         { \s -> LTokNotEq }             -- not equal operator

       -- Logical Operators (AND, &&, NOT, !, ||, OR, XOR)
       --
       <0> "&&"                         { \s -> LTokAndOp }             -- logical and
       <0> "!"                          { \s -> LTokNotOp }             -- negation operator
       <0> "||"                         { \s -> LTokOrOp }              -- logical or
       -- XOR and all string versions of logical operations
       -- are handled at ident function.

       -- Assignment Operators (=, :=)
       -- = is handled as comparison operators
       <0> ":="                         { \s -> LTokAssign }            -- assignment operator
       

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
       "check" -> LTokCheck
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
       "div" -> LTokIntDiv
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
       "into" -> LTokInto
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
       "right" -> LTokRight
       "schema" -> LTokSchema
       "select" -> LTokSelect
       "set" -> LTokSet
       "smallint" -> LTokSmallInt
       "straight_join" -> LTokStraightJoin
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
       "xor" -> LTokXOr
       ident' -> LTokIdent $ LIdentSimpleToken s
}
