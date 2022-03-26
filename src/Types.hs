
-- Accomponying implementation for notes on Information-FPublic Basics.
-- aslan@cs.au.dk

-- Type-based enforcement of NI
-- in the style of Volpano, Smith, Irvine
-- (aka Denning-style enforcement)

-- CHANGELOG:

-- 2016-04-07: Initial implementation closely matching Sec 3 of the notes


module Types where
import Imp
import qualified Data.String.Utils -- cabal install MissingH

data Level = Public | Secret deriving (Eq, Show)

--

flowsto :: Level -> Level -> Bool
flowsto Secret Public = False
flowsto _ _ = True


join :: Level -> Level -> Level
join Public Public = Public
join _ _     = Secret



-- convenient synonyms
(⊔) = join
(⊑) = flowsto

--


type Environment = VarName -> Level


-- Expression Typing
exprType :: Environment -> Expr -> Level
exprType _   (IntExpr _ ) = Public
exprType env (VarExpr x ) = env x
exprType env (BinOpExpr _ e1 e2) =
    join (exprType env e1) (exprType env e2)


-- We record the result of type checking a command
-- as a value of type TypeRes

data TypeRes = WellTyped | TypeError String
  deriving (Eq, Show)


-- Command Typing

cmdType :: Environment -> Level -> Cmd -> TypeRes

cmdType _Γ pc Skip = WellTyped

cmdType _Γ pc (Assign x e) =
    let ℓ = exprType _Γ e
     in if (pc ⊔ ℓ) ⊑ _Γ(x)
          then WellTyped
          else TypeError "assignment does not type check"
          -- TODO We could give more instructive error mesasges here
          -- e.g., by distinguishing the cases where program counter label is
          -- more restrictive than the level of the variable vs
          -- the expression that is being assigned

cmdType _Γ pc (Seq c1 c2) =
    case cmdType _Γ pc c1 of
        WellTyped         -> cmdType _Γ pc c2
        err               -> err

cmdType _Γ pc (If e c1 c2) =
    let ℓ   = exprType _Γ e
        pc' = pc ⊔ ℓ
     in case cmdType _Γ pc' c1 of
          WellTyped -> cmdType _Γ pc' c2
          err       -> err

cmdType _Γ pc (While e c) =
  let ℓ   = exprType _Γ e
      pc' = pc ⊔ ℓ
   in cmdType _Γ pc' c

-- For Haskell affectionados: TypeRes is a monad
-- and we could have rewritten the code in a way that
-- burries error checking into bind, but we explicitly
-- don't want to go there, for illustration purposes
--



-- EXAMPLES


-- dirty hack due to our naming convention

levelFromName x =
   if Data.String.Utils.endswith "_p" x
        then Public
        else Secret



-- Declare all Secret environment
allSecretEnv = \x -> Secret

-- another dirty hack: update function declared in Imp
-- happens to be polymorphic enough to apply to type enviroments
initEnv vars =
  foldl (\env var -> update env var (levelFromName var)) allSecretEnv vars


