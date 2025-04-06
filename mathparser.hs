-- Custom Language in Haskell

data Tree = Node Tree String Tree | Leaf String | Empty
 deriving (Eq, Show)

data Expr = Val Float
 -- Arithmetic
 | Add Expr Expr | Sub Expr Expr
 | Mul Expr Expr | Div Expr Expr
 | Exp Expr Expr 
 -- Comparison
 | Les Expr Expr | Gre Expr Expr 
 | Equ Expr Expr | Lte Expr Expr
 | Gte Expr Expr
 -- Logical
 | Not Expr | And Expr Expr
 | Ior Expr Expr | Xor Expr Expr
 deriving (Eq, Show)

boolToFloat :: Bool -> Float
boolToFloat b 
 | b = 1
 | not b = 0

-- Expr evaluation function
eval :: Expr -> Float
eval (Val f) = f
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a / eval b
eval (Exp a b) = eval a ** eval b

eval (Les a b) = boolToFloat (eval a < eval b)
eval (Gre a b) = boolToFloat (eval a > eval b)
eval (Equ a b) = boolToFloat (eval a == eval b)
eval (Gte a b) = boolToFloat (eval a >= eval b)
eval (Lte a b) = boolToFloat (eval a <= eval b)

eval (Not a) = if eval a /= 0 then 0 else 1
eval (And a b) = if eval a /= 0 && eval b /= 0 then 1 else 0
eval (Ior a b) = if eval a /= 0 || eval b /= 0 then 1 else 0
eval (Xor a b) = if (eval a /= 0 && eval b == 0) || (eval a == 0 && eval b /= 0) then 1 else 0 

-- Remove one layer of brackets, "(2+(5/3))" -> "2+(5/3)"
deBracket' :: String -> Int -> String
deBracket' [] _ = []
deBracket' (x:xs) n
 | x == '(' && n == 0 = deBracket' xs (n+1)
 | x == ')' && n == 1 = []
 | x == '(' = x : deBracket' xs (n+1)
 | x == ')' = x : deBracket' xs (n-1)
 | otherwise = x : deBracket' xs n

deBracket :: String -> String
deBracket s = deBracket' s 0

isDigit :: Char -> Bool
isDigit c = (c >= '0') && (c <= '9')

-- Check if a string consists of only positive or negative numbers, no spaces or anything
isNum :: String -> Bool
isNum str = case reads str :: [(Float, String)] of
 [(n, "")] -> True
 _         -> False

-- Check to see if the string is +, -, *, /, ^, (, ).
isArithmetic :: String -> Bool
isArithmetic s = s == "+" || s == "-" || s == "*" || s == "/" || s == "^"
isAdd :: String -> Bool
isAdd s = s == "+"
isSub :: String -> Bool
isSub s = s == "-"
isMul :: String -> Bool
isMul s = s == "*"
isDiv :: String -> Bool
isDiv s = s == "/"
isExp :: String -> Bool
isExp s = s == "^"

isBrc :: String -> Bool
isBrc s = length s > 1 && not (isNum s) && not (isOperator s)

isComparison :: String -> Bool
isComparison s = (s == "<") || (s == ">") || (s == "<=") ||(s == ">=") || (s == "=")
isLes :: String -> Bool
isLes s = s == "<"
isEqu :: String -> Bool
isEqu s = s == "="
isGre :: String -> Bool
isGre s = s == ">"
isLte :: String -> Bool
isLte s = s == "<="
isGte :: String -> Bool
isGte s = s == ">="

isLogical :: String -> Bool
isLogical s = (s == "!") || (s == "|") || (s == "&") || (s == "~")
isNot :: String -> Bool
isNot s = s == "!"
isIor :: String -> Bool
isIor s = s == "|"
isAnd :: String -> Bool
isAnd s = s == "&"
isXor :: String -> Bool
isXor s = s == "~"

isUnary :: String -> Bool
isUnary s = isNot s
isBinaryOp :: String -> Bool
isBinaryOp s = isOperator s && not (isUnary s)

isOperator :: String -> Bool
isOperator s = isArithmetic s || isComparison s || isLogical s

precedence :: String -> Int
precedence op
 | op == "-" = 1
 | op == "+" = 2
 | op == "*" = 3
 | op == "/" = 4
 | op == "^" = 5
 | isComparison op = 0
 | op == "!" = -1
 | op == "&" = -2
 | op == "|" = -3
 | op == "~" = -4
 | op == ":=" = -5

isVal :: String -> Bool
isVal s = isNum s || isBrc s

-- (2+5) -> ( 2 + 5 )
padExpr :: String -> String
padExpr (x:xs)
 | null xs = [x]
 | x == '.' || x == ' ' = x : padExpr xs
--  | xs!!0 == ' ' = x : padExpr xs
 | isDigit x && (isDigit (xs!!0) || xs!!0 == '.') = x : padExpr xs
 | otherwise = x : ' ' : padExpr xs
-- ( 2 + 5 ) -> (2+5)
unpadExpr :: String -> String
unpadExpr (x:xs)
 | null xs = [x]
 | x /= ' ' = x:unpadExpr xs
 | otherwise = unpadExpr xs

-- "2+2/(3-(7^2))" -> "2 + 2 / (3-(-7^2))"
padUnlessBrc' :: String -> Int -> String
padUnlessBrc' [] _ = []
padUnlessBrc' (x:xs) level
 | x == '(' = x : padUnlessBrc' xs (level+1)
 | x == ')' = x : padUnlessBrc' xs (level-1)
 | (isOperator [x] || x == ':') && level == 0 = ' ' : x : ' ' : padUnlessBrc' xs level -- A+B -> A + B
 | isDigit x || x == '.' = x : padUnlessBrc' xs level -- "3.13" -> "3.13", no changes, prevent "3 . 13"
 | otherwise = x : padUnlessBrc' xs level

padUnlessBrc :: String -> String
padUnlessBrc s = padUnlessBrc' s 0

joinMultiCharOperators :: [String] -> [String]
joinMultiCharOperators [] = []
joinMultiCharOperators [x] = [x]
joinMultiCharOperators [x,xs] = [x, xs]
joinMultiCharOperators (x:xs:xss)
 | not (isVal x) && not (isVal xs) = (x++xs) : joinMultiCharOperators xss
 | otherwise = x : joinMultiCharOperators (xs:xss)

-- Append to Tree
addToTree :: Tree -> String -> Tree
addToTree Empty ins
 | isVal ins || isUnary ins = Leaf ins
 | otherwise = error ("\nCannot have an operator go first: (" ++ ins ++ ")\n")      -- () -> A

addToTree (Leaf tVal) ins
 | isUnary tVal && isVal ins = Node (Leaf ins) tVal Empty
 | isOperator ins = Node (Leaf tVal) ins Empty
 | otherwise = error ("\nCannot append a val to another val as an operator on its own: (" ++ ins ++ ")\n")

addToTree (Node leftT tVal rightT) ins    -- Any Operator = #, Empty = (), Num or Brc = A,B,C...
 | isVal tVal && isOperator ins  = Node (Leaf tVal) ins Empty                       -- A >=> A # ()
 | isOperator tVal && (isVal ins || isUnary ins) && rightT == Empty = Node leftT tVal (Leaf ins)     -- A # () >=> A # B
 | isOperator tVal && isOperator ins && precedence ins <= precedence tVal = Node (Node leftT tVal rightT ) ins Empty -- A * B >=> (A * B) + C
 | isOperator tVal && isOperator ins && precedence ins > precedence tVal = Node leftT tVal (Node rightT ins Empty) -- A - B >=> A - (B / C)
 | leftT /= Empty && rightT /= Empty = Node leftT tVal (addToTree rightT ins)       -- A - B >=>  A - (B - ())
 | otherwise = error ("\nInvalid addition to the expression: (" ++ ins ++ ")\n")

-- Construct Syntax Tree
syntaxTree' :: Tree -> [String] -> Tree
syntaxTree' t [] = t
syntaxTree' t (x:xs) = syntaxTree' (addToTree t x) xs

syntaxTree :: String -> Tree
syntaxTree list = braceExpansion (syntaxTree' Empty (tokenize list))

tokenize :: String -> [String]
tokenize s = joinMultiCharOperators (words ( padUnlessBrc (unpadExpr s)))

-- Brace expansion
-- Leaf "3+2" -> Node (Leaf "3") "+" (Leaf "2")
braceExpansion :: Tree -> Tree
braceExpansion Empty = Empty
braceExpansion (Leaf b)
 | isBrc b = braceExpansion (syntaxTree (deBracket b))
 | otherwise = Leaf b
braceExpansion (Node left val right) = Node (braceExpansion left) val (braceExpansion right)

treeToExpr :: Tree -> Expr
treeToExpr (Leaf l) = Val (read l :: Float)
treeToExpr (Node left val right)
 | isAdd val = Add (treeToExpr left) (treeToExpr right)
 | isSub val = Sub (treeToExpr left) (treeToExpr right)
 | isMul val = Mul (treeToExpr left) (treeToExpr right)
 | isDiv val = Div (treeToExpr left) (treeToExpr right)
 | isExp val = Exp (treeToExpr left) (treeToExpr right)

 | isLes val = Les (treeToExpr left) (treeToExpr right)
 | isEqu val = Equ (treeToExpr left) (treeToExpr right)
 | isGre val = Gre (treeToExpr left) (treeToExpr right)
 | isLte val = Lte (treeToExpr left) (treeToExpr right)
 | isGte val = Gte (treeToExpr left) (treeToExpr right)

 | isNot val = Not (treeToExpr left)
 | isIor val = Ior (treeToExpr left) (treeToExpr right)
 | isAnd val = And (treeToExpr left) (treeToExpr right)
 | isXor val = Xor (treeToExpr left) (treeToExpr right)

evaluateTree :: Tree -> Float
evaluateTree t = eval (treeToExpr t)

main :: IO ()
main = do
    putStrLn "Math Expression :"
    expr <- getLine  -- Read input from the user
    let result = show (evaluateTree ( syntaxTree expr ))  -- Evaluate input
    putStrLn result  -- Print the result
