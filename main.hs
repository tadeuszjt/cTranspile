import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Tok
import System.Environment
import qualified Cgen as C

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

data Program = Program [Statement] deriving (Show)
data Statement = Statement String deriving (Show)

statement :: Parser Statement
statement = do
	id <- ident
	semi
	return $ Statement id

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

build :: Program -> C.CFileState
build (Program statements) = do
	C.ensureInclude "stdio.h"
	C.ensureInclude "stdio.h"
	C.ensureInclude "stdint.h"
	C.addFuncDef $ C.FuncDef "int" "main" (map (\(Statement str) -> str) statements)

main :: IO ()
main = do
	args <- getArgs
	p <- parseFile program (head args)
	let cFile = C.emptyCFile
	C.putCFile $ C.execCFileState (build p) cFile
	
