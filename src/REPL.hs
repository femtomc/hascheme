module REPL (readExpr, eval) where

import Parser

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (VString contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (VInt k) = show k
showVal (VBool True) = "#t"
showVal (VBool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "("
    ++ unwordsList head
    ++ "."
    ++ showVal tail
    ++ ")"

instance Show LispVal where show = showVal

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> VString $ "No match: " ++ show err
  Right val -> val

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = VInt $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (VInt n) = n
unpackNum (VString n) =
  let parsed = reads n :: [(Integer, String)]
   in if null parsed
        then 0
        else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (VBool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(VString _) = val
eval val@(VInt _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
