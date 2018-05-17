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

skip =spaces -# accept "skip" # require ";" >-> (\_ -> Skip)
begin = spaces -# accept "begin" -# iter parse #- require "end" >-> (\x -> Begin x)
ifstatement =spaces -# accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> (\((ex,s1),s2) -> If ex s1 s2)
while =spaces -# accept "while" -# Expr.parse #- require "do" # parse >-> (\(ex, s) -> While ex s)
readstatement =spaces -# accept "read" -# word #- require ";" >-> (\x -> Read x)
write = spaces -# accept "write" -# Expr.parse #- require ";" >-> (\x -> Write x)
comment = spaces -# accept "--" -# iter notNewline # iter newline >-> (\_ -> Comment)

notNewline :: Parser Char
notNewline [] = Nothing
notNewline (x:xs)
 | x == '\n' = Nothing
 | otherwise = Just (x,xs)

newline :: Parser Char
newline [] = Nothing
newline (x:xs)
 | x == '\n' = Just (x,xs)
 | otherwise = Nothing




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

makeSpace n = replicate (2*n) ' '
toStringhelp n (Assignment v e)= makeSpace n ++ v ++ " := "++ Expr.toString e ++ ";\n"
toStringhelp  n (Skip) = makeSpace n ++ "skip;\n"
toStringhelp n (Begin x) = makeSpace n ++ "begin\n" ++ (concat $ map (toStringhelp (n+1)) x) ++ makeSpace n ++ "end\n"
toStringhelp n (If expr s1 s2) =makeSpace n ++ "if " ++  Expr.toString expr ++ " then\n" ++ toStringhelp (n+1) s1 ++ makeSpace n ++ "else\n" ++ toStringhelp (n+1) s2
toStringhelp n (While expr s1) = makeSpace n ++ "while " ++ Expr.toString expr ++ " do\n" ++ toStringhelp (n+1) s1
toStringhelp n (Read str) = makeSpace n ++ "read " ++ str ++ ";\n"
toStringhelp n (Write expr) = makeSpace n ++ "write " ++ Expr.toString expr ++ ";\n"


instance Parse Statement where
  parse = assignment ! skip ! begin ! ifstatement ! while ! readstatement ! write ! comment
  toString = toStringhelp 0
