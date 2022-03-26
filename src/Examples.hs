module Examples where

import Imp
import Types


-- Some helper function to run our program for at most 100 steps



-- EXAMPLES

-- Example "initialized-to-zero" memory

mZ = \_ -> 0


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


exercise2 =  While (VarExpr "y_s") Skip

---
-- shortcut to check programs with varsOfInterest
-- note that starting program counter label is Public

varsOfInterest = ["x_p", "y_s"] -- only two variables; do add more based on examples
                                -- (or better infer them from the source)

run100 = runF 100 varsOfInterest -- obs: fuel argument of 100 steps hardcoded


checkWithVarsOfInterest = cmdType (initEnv varsOfInterest) Public


runTyped p m = do 
  let tc = checkWithVarsOfInterest p in 
    case tc of 
      WellTyped -> run100 (p, m)
      TypeError msg -> print msg

runUntyped p m = run100 (p, m)
  
