module Environment where

{-
  Helpful things for environments and variables.
-}

import Control.Monad (liftM)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Types
import PrettyPrint

----------------------------------------
-- Lookups
----------------------------------------

existsStateAspect :: Environment -> GlobalStateAspect -> Bool
existsStateAspect env aspect = aspect `elem` (env_stateAspects env)

existsType :: Environment -> TypeName -> Bool
existsType env tn = isJust $ findType env tn

existsVar :: Environment -> Var -> Bool
existsVar env var = isJust $ findVarType env var

findActionInfo :: Environment -> ActionName -> Maybe ActionInfo
findActionInfo env act = M.lookup act $ env_actions env

findActionType :: Environment -> ActionTypeName -> Maybe ActionType
findActionType env atn = M.lookup atn $ env_actionTypes env

findStruct :: Environment -> TypeName -> Maybe Struct
findStruct env tn = M.lookup tn $ env_structs env

findSubroutineType :: Environment -> SubroutineName -> Maybe SubroutineType
findSubroutineType env sub = M.lookup sub $ env_subTypes env

findType :: Environment -> TypeName -> Maybe (Maybe String)
findType env tn = M.lookup tn $ env_types env

findVarInfo :: Environment -> Var -> Maybe VarInfo
findVarInfo env v = M.lookup v $ env_vars env

findVarType :: Environment -> Var -> Maybe Type
findVarType env v = liftM vi_type $ findVarInfo env v

mustFindActionInfo :: Environment -> ActionName -> ActionInfo
mustFindActionInfo env meth =
  case findActionInfo env meth of
    Just mi -> mi
    _ -> error $ "Could not find action named: " ++ show meth

mustFindActionType :: Environment -> ActionTypeName -> ActionType
mustFindActionType env atn =
  case findActionType env atn of
    Just at -> at
    _ -> error $ "Could not find action type named: " ++ show atn

mustFindStruct :: Environment -> TypeName -> Struct
mustFindStruct env tn =
  case findStruct env tn of
    Just s -> s
    _ -> error $ "Could not find struct info for type: " ++ show tn

mustFindSubroutineType :: Environment -> SubroutineName -> SubroutineType
mustFindSubroutineType env sub =
  case findSubroutineType env sub of
    Just st -> st
    _ -> error $ "Could not find type of subroutine: " ++ show sub

mustFindVarInfo :: Environment -> Var -> VarInfo
mustFindVarInfo env v =
  case findVarInfo env v of
    Just vi -> vi
    _ -> error $ "Could not find variable: " ++ printVar v

mustFindVarType :: Environment -> Var -> Type
mustFindVarType env v =
  case findVarType env v of
    Just t -> t
    _ -> error $ "Could not find type of variable: " ++ printVar v

----------------------------------------
-- More involved
----------------------------------------

{-
getLhsType :: Environment -> Lhs -> Type
getLhsType env lhs =
  case lhs of
    LhsVarField v [] ->
      let vi = mustFindVarInfo env v
      in Atomic $ vi_type vi
    LhsVarField v _ -> dontknow
    LhsTuple lhss ->
      let ts = map (getLhsType env) lhss
      in Product ts
    LhsIgnore -> dontknow
  where
    dontknow = Atomic Inferred
-}

----------------------------------------
-- Construction/modification
----------------------------------------

emptyEnvironment :: Environment
emptyEnvironment =
  Environment "" ProtocolAuto M.empty M.empty M.empty M.empty S.empty M.empty
    M.empty [] M.empty

-- Add a variable to an environment, raise error if it already exists.
addVar :: Var -> VarInfo -> Environment -> Environment
addVar v vi env =
  let vars = env_vars env
  in case M.lookup v vars of
    Just _ -> error $ "Variable " ++ printVar v ++ " already exists in environment"
    Nothing -> env { env_vars = M.insert v vi vars }

-- Create a fresh variable.
allocateMiscVar :: Type -> Environment -> (Var, Environment)
allocateMiscVar typ env =
  let vars = M.keys $ env_vars env
      index = foldr max 0 $ vars >>= extract
      extract v =
        case v of
          MiscVar i -> [i+1]
          _ -> []
      var = MiscVar index
      info = VarInfo typ []
  in (var, addVar var info env)
