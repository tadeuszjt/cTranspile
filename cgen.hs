module Cgen where

tabs n = replicate n '\t'

data Statement = Statement String
putStatement indent (Statement str) =
	putStrLn $ tabs indent ++ "puts(\"" ++ str ++ "\");"

data Block = Block {statements :: [Statement]}
putBlock indent (Block stmts) = do
	putStrLn $ tabs indent ++ "{"
	mapM_ (putStatement $ indent+1) stmts
	putStrLn $ tabs indent ++ "}"

data Includes = Includes [String]
putIncludes (Includes ss) =
	mapM_ (\x -> putStrLn $ "#include \"" ++ x ++ "\"") ss

data FuncHdr = FuncHdr {retType :: String, name :: String}
putFuncHdr f =
	putStr $ retType f ++ " " ++ name f ++ "()"

data FuncDef = FuncDef {header :: FuncHdr, body :: Block}
putFuncDef f = do
	putFuncHdr (header f)
	putBlock 0 (body f)

data CFile = CFile {includes :: Includes, funcDefs :: [FuncDef]}
putCFile :: CFile -> IO ()
putCFile (CFile includes funcDefs) = do
	putIncludes includes
	putStrLn ""
	mapM_ (\x -> do putFuncHdr x; putStrLn ";") $ map header funcDefs
	putStrLn ""
	mapM_ putFuncDef funcDefs

addFuncDef :: CFile -> FuncDef -> CFile
addFuncDef cFile funcDef =
	CFile {
		includes = includes cFile,
		funcDefs = funcDefs cFile ++ [funcDef]
	}

addStatement :: CFile -> String -> Statement -> CFile
addStatement cFile funcName statement =
	CFile {
		includes = includes cFile,
		funcDefs = [ if (name $ header x) == funcName then FuncDef (header x) (Block $ (statements $ body x) ++ [statement]) else x | x <- funcDefs cFile]
	}

