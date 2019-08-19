import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Tok
import System.Environment
import Cgen as C

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

data Program = Program [String] deriving (Show)

statement :: Parser String
statement = do
	id <- ident
	semi
	return id

program :: Parser Program
program = do
	whiteSpace
	statements <- many1 statement
	return $ Program statements
	
parseFile :: Parser Program -> String -> IO Program
parseFile parser filename = do
	content <- readFile filename
	return $ case parse parser "" content of
		Left e -> error $ show e
		Right r -> r

main :: IO ()
main = do
	args <- getArgs
	(Program p) <- parseFile program (head args)
	C.putCFile (C.Includes ["stdio.h"]) [C.FuncDef {
			header = C.FuncHdr {
				retType = "int",
				name = "main"
			},
			body = C.Block $ map C.Statement p
		}]
	
