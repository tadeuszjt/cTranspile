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

data Statement = PrintS String | Call String deriving Show
data Define = Define String [Statement] deriving Show
data Step = StepStatement Statement | StepDefine Define deriving Show
data Program = Program [Step] deriving Show


printS :: Parser Statement
printS = do
	lit <- stringLiteral
	return $ PrintS lit

call :: Parser Statement
call = do
	id <- ident
	return $ Call id

statement :: Parser Statement
statement =
	printS <|> call

define :: Parser Define
define = do
	name <- ident
	colon
	statements <- commaSep statement
	return $ Define name statements

step :: Parser Step
step =
	try (do d <- define; return $ StepDefine d) <|> 
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
		PrintS str -> do C.ensureInclude "stdio.h"; C.addStatement funcName (C.Print str)
		Call str -> do C.addStatement funcName (C.Call str)
		
buildDefine :: Define -> C.CFileState
buildDefine (Define name statements) = do
	C.addFuncDef name $ C.FuncDef "void" [] 
	mapM_ (buildStatement name) statements

buildStep :: Step -> C.CFileState
buildStep step = case step of
	StepStatement s -> buildStatement "main" s
	StepDefine    d -> buildDefine d

build :: Program -> C.CFileState
build (Program steps) =
	mapM_ buildStep steps

main :: IO ()
main = do
	args <- getArgs
	p <- parseFile (head args)
	C.putCFile $ C.execCFileState (build p) C.emptyCFile
