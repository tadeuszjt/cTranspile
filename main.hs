import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Tok
import System.Environment

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

cPuts str =
	"puts(\"" ++ str ++ "\");"

data CIncludes = CIncludes {strings :: [String]}
printCIncludes ci =
	foldr1 (++) $ map (\x -> "#include \"" ++ x ++ "\"") (strings ci)

data CStatement = CStatement {str :: String}
printCStatement cs =
	"printf(\"" ++ str cs ++ "\\n\")"

data CBlock = CBlock {statements :: [CStatement]}
printCBlock cb =
	"{\n" ++ foldr1 (++) (map (\x -> "\t" ++ printCStatement x ++ ";\n") (statements cb)) ++ "}"

data CFuncHdr = CFuncHdr {retType :: String, name :: String}
printCFuncHdr cf =
	retType cf ++ " " ++ name cf ++ "()"

data CFuncDef = CFuncDef {header :: CFuncHdr, body :: CBlock}
printCFuncDef fd =
	printCFuncHdr (header fd) ++ " " ++ printCBlock (body fd)

data CFile = CFile {includes :: CIncludes, funcDefs :: [CFuncDef]}
printCFile :: CFile -> IO ()
printCFile cf =
	printCIncludes (includes cf) ++ "\n"

main :: IO ()
main = do
	args <- getArgs
	prog <- parseFile program (head args)
	putStrLn $ printCFile $ CFile {
		includes = CIncludes ["stdio.h"],
		funcDefs = [
			CFuncDef {
				header = CFuncHdr "int" "main",
				body = CBlock [CStatement "Benis"]
			}
		]
	}

