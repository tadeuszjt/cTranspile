module Cgen where

import Control.Monad.State
import qualified Data.Set as Set

tabs n = replicate n '\t'
quotes s = "\"" ++ s ++ "\""
wrap l r x = l ++ x ++ r

data Statement = Print String | Call String

putStatement :: Int -> Statement -> IO ()
putStatement indent statement =
	putStrLn $ (tabs indent) ++ case statement of
		Print str -> wrap "puts(" ");" (quotes str) 
		Call str -> str ++ "();"

data FuncDef = FuncDef {
	retType :: String,
	name    :: String,
	body    :: [Statement]
}

putFuncHdr :: FuncDef -> IO ()
putFuncHdr funcDef = do
	putStr $ retType funcDef ++ " " ++ name funcDef ++ "()"
	
putFuncDef :: FuncDef -> IO ()
putFuncDef funcDef = do
	putFuncHdr funcDef
	putStrLn " {"
	mapM_ (putStatement 1) $ body funcDef
	putStrLn "}"

data CFile = CFile {
	includes :: Set.Set String,
	funcDefs :: [FuncDef]
}

putCFile :: CFile -> IO ()
putCFile cFile = do
	mapM_ (\x -> putStrLn $ "#include " ++ quotes x) $ Set.toList (includes cFile)
	putStrLn ""
	mapM_ (\x -> do putFuncHdr x; putStrLn ";") $ funcDefs cFile
	putStrLn "" 
	mapM_ (\x -> do putFuncDef x; putStrLn "") $ funcDefs cFile

emptyCFile = CFile Set.empty []

type CFileState = State CFile ()

execCFileState cFileState =
	execState cFileState

addFuncDef :: FuncDef -> CFileState
addFuncDef funcDef = do
	(CFile includes funcDefs) <- get
	put $ CFile includes (funcDefs ++ [funcDef])

ensureInclude :: String -> CFileState
ensureInclude path = do
	(CFile includes funcDefs) <- get
	put $ CFile (Set.insert path includes) funcDefs
	
