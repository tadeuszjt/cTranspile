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
	try call <|>
	try int <|>
	str

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

buildStatement :: String -> Statement -> C.CFileState
buildStatement funcName s =
	case s of
		Call str -> do C.addStatement funcName (C.Call str)
		Str str -> do C.addStatement funcName (C.Print str); C.include "stdio.h"
		Num n -> do C.addStatement funcName (C.PrintNum n); C.include "stdio.h"
		
buildDefine :: Step -> C.CFileState
buildDefine (Define name statements) = do
	C.addFuncDef name $ C.FuncDef "void" [] 
	mapM_ (buildStatement name) statements

buildStep :: Step -> C.CFileState
buildStep step = case step of
	StepStatement s -> buildStatement "main" s
	d@(Define _ _) -> buildDefine d

build :: Program -> C.CFileState
build (Program steps) =
	mapM_ buildStep steps

main :: IO ()
main = do
	args <- getArgs
	p <- parseFile (head args)
	C.putCFile $ C.execCFileState (build p) C.emptyCFile
