-- Simple interpreter for imperative language
-- to accompony lecture notes on information flow basics

-- aslan@cs.au.dk


-- CHANGELOG:

-- 2016-04-07: Initial interpreter that matches Section 1 of the notes
-- Of particular relevance are definitions of Configuration,
-- evaluation of expressions exprEval and the implementation step of
-- small-step transition.

-- Techincal note: the notes define our semantics as _relations_, whereas
-- we implement them here as pure functions. This implementation choice is OK
-- because our language is deterministic (and makes many things in the implementation
-- easier).

-- The example programs at the bottom of the file correspond to examples in
-- the notes.


module Imp where

type VarName = String
type Value   = Integer

data BinOp = Plus | Minus | Times
              deriving (Eq,Show)

data Expr  = IntExpr Value | VarExpr VarName | BinOpExpr BinOp Expr Expr
           deriving (Eq, Show)

data Cmd = Skip | Assign VarName Expr | Seq Cmd Cmd
         | If Expr Cmd Cmd | While Expr Cmd
         | Stop
           deriving (Eq, Show)

-- Memory is a (total?) function from
-- Variables to Values

type Memory = VarName -> Value


-- memory update
update m x v = \y -> if y == x then v else m y


type Configuration = (Cmd, Memory)


-- (Big-step) semantics of expressions
--

-- exprEval e m returs the result of evaluating expression e
-- in memory m

exprEval :: Expr -> Memory -> Value
exprEval (IntExpr n) _ = n
exprEval (VarExpr x) m = m x
exprEval (BinOpExpr binop e1 e2) m =
    let
        v1 = exprEval e1 m
        v2 = exprEval e2 m
    in (binOpSem binop) v1 v2
      where
            -- auxiliary function that takes
            -- expressions from our language to functions
            -- on values
            binOpSem Plus   = (+)
            binOpSem Minus  = (-)
            binOpSem Times  = (*)


-- SMALL-STEP SEMANTICS OF COMMANDS

-- step is a function that takes one configuration
-- to the next one

step :: Configuration -> Configuration

step (Skip, m ) = (Stop, m)

step (Assign x e, m) =
    let v = exprEval e m
    in (Stop, update m x v)

step (Seq c1 c2, m) =
    let (c1', m') = step (c1, m)
    in case c1' of
          Stop -> (c2, m')
          _    -> (Seq c1' c2, m')

step (If e c1 c2, m) =
    case (exprEval e m) of
        0 -> (c2, m)
        _ -> (c1, m)

step (While e c, m) = (If e (Seq c (While e c)) Skip, m)



-- END OF SEMANTICS


-- INFRASTRUCTURE

-- Function evalF takes a "fuel" argument n and runs our command through
-- n steps. The fuel is a hack here for educational purposes, so we can
-- "timeout" possibly diverging programs


data Result = Finished Memory | OutOfFuel

evalF :: Integer -> Configuration -> Result
evalF 0 _ = OutOfFuel
evalF n config =
    -- (at) notation @ means config' is a synonym for (c', m')
    let config'@(c', m') = step config
    in case c' of
        Stop -> Finished m'
        _    -> evalF (n-1) config'


-- print variables in vars on screen

printMem :: Memory -> [VarName] -> IO ()
printMem m vars =
    mapM_ ( \x ->  putStrLn (x ++ ": " ++  (show (m x))) ) vars


-- run program c with fuel n and print the variable values

runF n vars c  =
    case evalF n c of
        OutOfFuel  -> print "OutOfFuel"
        Finished m -> printMem m vars


-- Some helper function to run our program for at most 100 steps

run100 = runF 100



-- EXAMPLES

-- Example "initialized-to-zero" memory

mZ = \x -> 0


-- Some memory environments with different secrets

m0 = update mZ "y_s" 0
m1 = update mZ "y_s" 1

-- Sample programs in AST -- because someone is lazy to implement a parser


-- x_p := y_s

example1 = Assign "x_p" (VarExpr "y_s")

-- y_s := 42; x_p := y_s

example3 = Seq (Assign "y_s" (IntExpr 42))
               (Assign "x_p" (VarExpr "y_s"))

--

example4 = If (VarExpr "y_s")               -- if y_s
                (Assign "x_p" (IntExpr 1))  --     then x_p := 1
                (Assign "x_p" (IntExpr 0))  --     else x_p := 0


exercise2 =  While (VarExpr "y_s") (Skip)

---

varsOfInterest = ["x_p", "y_s"]

runEx1  = run100 varsOfInterest (example1, mZ)

runEx4a = run100 varsOfInterest (example4, m0)

runEx4b = run100 varsOfInterest (example4, m1)

runExr2 = run100 varsOfInterest (exercise2, m1) -- try changing the memory to m0


-- run our example;

runExample = runExr2 -- try substituting for different runs here
