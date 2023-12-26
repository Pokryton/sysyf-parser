module Lexer where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

style :: P.LanguageDef st
style = emptyDef
         { P.commentStart    = "/*"
         , P.commentEnd      = "*/"
         , P.commentLine     = "//"
         , P.nestedComments  = False
         , P.identStart      = letter <|> char '_'
         , P.identLetter     = alphaNum <|> char '_'
         , P.reservedOpNames = ["+", "-", "*", "/", "%", "!", "<", ">"
                               , "<=", ">=", "==", "!=", "&&", "||"]
         , P.reservedNames   = ["void", "int", "float", "if", "while"
                               , "break", "continue"]
         , P.caseSensitive   = True
         }

lexer = P.makeTokenParser style

integer = P.integer lexer
float = P.float lexer
operator = P.operator lexer
identifier = P.identifier lexer

semi = P.semi lexer
comma = P.comma lexer

parens = P.parens lexer
brackets = P.brackets lexer
braces = P.braces lexer

commaSep = P.commaSep lexer
semiSep = P.semiSep lexer

reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
