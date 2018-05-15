module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Comment
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" # require ";" >-> (\_ -> Skip)
begin = accept "begin" -# iter parse #- require "end" >-> (\x -> Begin x)
ifstatement = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> (\((ex,s1),s2) -> If ex s1 s2)
while = accept "while" -# Expr.parse #- require "do" # parse >-> (\(ex, s) -> While ex s)
readstatement = accept "read" -# word #- require ";" >-> (\x -> Read x)
write = accept "write" -# Expr.parse #- require ";" >-> (\x -> Write x)
comment = accept "--" -# iter notNewline >-> (\_ -> Comment)

notNewline :: Parser Char
notNewline [] = Nothing
notNewline (x:xs)
 | x == '\n' = Nothing
 | otherwise = Just (x,xs)


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Begin xs : stmts) dict input =
    exec (xs++stmts) dict input
exec (Skip:stmts) dict input =
    exec stmts dict input
exec (Assignment s expr : stmts) dict input =
    exec stmts (Dictionary.insert (s ,Expr.value expr dict) dict) input
exec (While cond s: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (s:(While cond s):stmts) dict input
    else exec stmts dict input
exec (Read str : stmts) dict (i:input) =
    exec stmts (Dictionary.insert (str,i) dict) input
exec (Write expr : stmts) dict input =
    Expr.value expr dict : exec stmts dict input



instance Parse Statement where
  parse = assignment ! skip ! begin ! ifstatement ! while ! readstatement ! write ! comment
  toString (Assignment v e)= v ++ " := "++ Expr.toString e ++ ";\n"
  toString (Skip) = "skip;\n"
  toString (Begin x) = "begin\n" ++ (concat $ map ((\s -> "  "++s) . toString) x) ++ "end\n"
  toString (If expr s1 s2) = "if " ++ Expr.toString expr ++ " then\n  " ++ 
