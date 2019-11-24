module Cgen where

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map

quotes :: String -> String
quotes s = "\"" ++ s ++ "\""

wrap :: String -> String -> String -> String
wrap l r x =
	l ++ x ++ r

putTabs :: Int -> IO ()
putTabs i =
	putStr $ replicate i '\t'

data Statement
	= Print String
	| Call String
	| PrintNum Int
	deriving Show

data FuncDef = FuncDef {
	retType :: String,
	body    :: [Statement]
} deriving Show

data CFile = CFile {
	includes :: Set.Set String,
	funcDefs :: Map.Map String FuncDef
} deriving Show

type CFileState = State CFile () 


emptyCFile = CFile Set.empty (Map.singleton "main" $ FuncDef "int" [])

putStatement :: Int -> Statement -> IO ()
putStatement indent statement = do
	putTabs indent
	putStrLn $ case statement of
		Print str -> wrap "puts(" ");" $ quotes str
		Call str -> str ++ "();"
		PrintNum n -> wrap "printf(\"%d\\n\", "  ");" $ show n 

putFuncHdr :: String -> FuncDef -> IO ()
putFuncHdr name def =
	putStr $ retType def ++ " " ++ name ++ "()"
	
putFuncDef :: String -> FuncDef -> IO ()
putFuncDef name def = do
	putFuncHdr name def
	putStrLn " {"
	mapM_ (putStatement 1) $ body def
	putStrLn "}"

putCFile :: CFile -> IO ()
putCFile cFile = do
	mapM_ (\x -> putStrLn $ "#include " ++ quotes x) $ Set.toList (includes cFile)
	putStrLn ""
	let defsList = Map.toList $ funcDefs cFile
	mapM_ (\(name, def) -> do putFuncHdr name def; putStrLn ";") defsList
	putStrLn ""
	mapM_ (\(name, def) -> do putFuncDef name def; putStrLn "") defsList

addFuncDef :: String -> FuncDef -> CFileState
addFuncDef name def = do
	defs <- gets funcDefs
	modify $ \s -> s {funcDefs = Map.insert name def defs}

addStatement :: String -> Statement -> CFileState
addStatement funcName statement = do
	defs <- gets funcDefs
	case Map.lookup funcName defs of
		Nothing  -> error "def not found"
		Just def -> modify $ \s -> s {
			funcDefs = Map.insert funcName (def {body = body def ++ [statement]}) defs
		}

include :: String -> CFileState
include path = do
	incs <- gets includes
	modify $ \s -> s { includes = Set.insert path incs }

execCFileState cFileState =
	execState cFileState


