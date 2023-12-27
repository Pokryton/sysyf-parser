module Lexer where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

def :: P.LanguageDef st
def = emptyDef
        { P.commentStart    = "/*"
        , P.commentEnd      = "*/"
        , P.commentLine     = "//"
        , P.nestedComments  = False
        , P.identStart      = letter <|> char '_'
        , P.identLetter     = alphaNum <|> char '_'
        , P.reservedNames   = [ "void", "int", "float", "const", "if", "else"
                              , "return", "while" , "break" , "continue"]
        , P.caseSensitive   = True
        }

lexer = P.makeTokenParser def

integer     = P.integer lexer
float       = P.float lexer
identifier  = P.identifier lexer
semi        = P.semi lexer
comma       = P.comma lexer
whiteSpace  = P.whiteSpace lexer

parens      = P.parens lexer
brackets    = P.brackets lexer
braces      = P.braces lexer
commaSep    = P.commaSep lexer

reserved    = P.reserved lexer
symbol      = P.symbol lexer
