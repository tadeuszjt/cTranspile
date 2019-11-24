module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Tok

lexer :: Tok.TokenParser()
lexer = Tok.makeTokenParser emptyDef {
	Tok.reservedOpNames = ["+", "-", "*", "/"],
	Tok.reservedNames = []
}

ident = Tok.identifier lexer	
semi = Tok.semi lexer
integer = Tok.integer lexer
float = Tok.float lexer
whiteSpace = Tok.whiteSpace lexer
stringLiteral = Tok.stringLiteral lexer
colon = Tok.colon lexer
commaSep = Tok.commaSep lexer

data Statement
	= Call String
	| Str String
	| Num Int
	deriving Show

data Step
	= StepStatement Statement
	| Define String [Statement]
	deriving Show

data Program = Program [Step] deriving Show


call :: Parser Statement
call = do
	id <- ident
	return $ Call id

str :: Parser Statement
str = do
	s <- stringLiteral
	return $ Str s

int :: Parser Statement
int = do
	n <- integer
	return $ Num (fromInteger n)

statement :: Parser Statement
statement =
	try call <|> int <|> str

define :: Parser Step
define = do
	name <- ident
	colon
	statements <- commaSep statement
	return $ Define name statements

step :: Parser Step
step =
	try define <|> 
	(do s <- statement; return $ StepStatement s)

program :: Parser Program
program = do
	steps <- many1 (do s <- step; semi; return s)
	return $ Program steps

parseFile :: String -> IO Program
parseFile filename = do
	content <- readFile filename
	return $ case parse program "" content of
		Left e -> error $ show e
		Right r -> r
