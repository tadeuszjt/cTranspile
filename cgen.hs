module Cgen where

tabs n = replicate n '\t'

data Statement = Statement {str :: String}
putStatement i s =
	putStrLn $ tabs i ++ "puts(\"" ++ str s ++ "\");"

data Block = Block {statements :: [Statement]}
putBlock i b = do
	putStrLn $ tabs i ++ "{"
	mapM_ (putStatement $ i+1) $ statements b
	putStrLn $ tabs i ++ "}"

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

putCFile :: Includes -> [FuncDef] -> IO ()
putCFile includes funcDefs = do
	putIncludes includes
	putStrLn ""
	mapM_ (\x -> do putFuncHdr x; putStrLn ";") $ map header funcDefs
	putStrLn ""
	mapM_ putFuncDef funcDefs
