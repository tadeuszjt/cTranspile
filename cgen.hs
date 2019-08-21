module Cgen where

import Control.Monad.State
import qualified Data.Set as Set

tabs n = replicate n '\t'
quotes s = "\"" ++ s ++ "\""
wrap l r x = l ++ x ++ r

data Statement = Print String | Call String deriving Show

putStatement :: Int -> Statement -> IO ()
putStatement indent statement =
	putStrLn $ (tabs indent) ++ case statement of
		Print str -> wrap "puts(" ");" (quotes str) 
		Call str -> str ++ "();"

data FuncDef = FuncDef {
	retType :: String,
	name    :: String,
	body    :: [Statement]
} deriving Show

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
} deriving Show

putCFile :: CFile -> IO ()
putCFile cFile = do
	mapM_ (\x -> putStrLn $ "#include " ++ quotes x) $ Set.toList (includes cFile)
	putStrLn ""
	mapM_ (\x -> do putFuncHdr x; putStrLn ";") $ funcDefs cFile
	putStrLn "" 
	mapM_ (\x -> do putFuncDef x; putStrLn "") $ funcDefs cFile

emptyCFile = CFile Set.empty [FuncDef "int" "main" []]

type CFileState = State CFile () 

execCFileState cFileState =
	execState cFileState

addFuncDef :: FuncDef -> CFileState
addFuncDef funcDef = do
	cFile <- get
	put cFile {funcDefs = funcDefs cFile ++ [funcDef]}

addStatement :: String -> Statement -> CFileState
addStatement funcName statement = do
	cFile <- get
	let funcDefs' = map (\x -> if name x == funcName then x {body = body x ++ [statement]} else x) funcDefs
	put cFile {funcDefs = funcDefs'}
	
ensureInclude :: String -> CFileState
ensureInclude path = do
	cFile <- get
	put cFile {includes = Set.insert path $ includes cFile}
	
