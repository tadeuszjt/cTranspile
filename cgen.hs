module Cgen where

import Control.Monad.State

tabs n = replicate n '\t'
quotes s = "\"" ++ s ++ "\""
wrap l r x = l ++ x ++ r

data FuncDef = FuncDef {
	retType :: String,
	name    :: String,
	body    :: [String]
}

putFuncHdr :: FuncDef -> IO ()
putFuncHdr funcDef = do
	putStr $ retType funcDef ++ " " ++ name funcDef ++ "()"
	
putFuncDef :: FuncDef -> IO ()
putFuncDef funcDef = do
	putFuncHdr funcDef
	putStrLn "{"
	mapM_ (\x -> putStrLn $ tabs 1 ++ wrap "puts(" ");" (quotes x)) $ body funcDef
	putStrLn "}"

data CFile = CFile {
	includes :: [String],
	funcDefs :: [FuncDef]
}

putCFile :: CFile -> IO ()
putCFile cFile = do
	mapM_ (\x -> putStrLn $ "#include " ++ quotes x) $ includes cFile
	putStrLn "" 
	mapM_ (\x -> do putFuncHdr x; putStrLn ";") $ funcDefs cFile
	putStrLn "" 
	mapM_ (\x -> do putFuncDef x; putStrLn "") $ funcDefs cFile

addFuncDef :: FuncDef -> State CFile ()
addFuncDef funcDef = do
	(CFile includes funcDefs) <- get
	put $ CFile includes (funcDefs ++ [funcDef])

addInclude :: String -> State CFile ()
addInclude include = do
	(CFile includes funcDefs) <- get
	put $ CFile (includes ++ [include]) funcDefs
	
