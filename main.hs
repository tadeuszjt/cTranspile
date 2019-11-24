import System.Environment
import qualified Cgen as C
import qualified Parser as P

buildStatement :: String -> P.Statement -> C.CFileState
buildStatement funcName s =
	case s of
		P.Call str -> do C.addStatement funcName (C.Call str)
		P.Str str -> do C.addStatement funcName (C.Print str); C.include "stdio.h"
		P.Num n -> do C.addStatement funcName (C.PrintNum n); C.include "stdio.h"
		
buildDefine :: P.Step -> C.CFileState
buildDefine (P.Define name statements) = do
	C.addFuncDef name $ C.FuncDef "void" [] 
	mapM_ (buildStatement name) statements

buildStep :: P.Step -> C.CFileState
buildStep step = case step of
	P.StepStatement s -> buildStatement "main" s
	d@(P.Define _ _) -> buildDefine d

build :: P.Program -> C.CFileState
build (P.Program steps) =
	mapM_ buildStep steps

main :: IO ()
main = do
	args <- getArgs
	p <- P.parseFile (head args)
	C.putCFile $ C.execCFileState (build p) C.emptyCFile
