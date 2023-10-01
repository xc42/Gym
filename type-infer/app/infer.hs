module TypeInfer where

import LangCore

import Control.Monad.IoRef
import qualified Data.Map as Map
import Text.Format

type Frame a        = Map.Map String a
type Environment a  = [Frame]
type Constrains     = [(LType, LType)]

lookup :: Environment a -> String -> a
lookup [] name          = error $ format "no found for var : {0}" name
lookup frame:rest name  = Map.findWithDefault (lookup rest name) name frame

extend :: Environment a -> Frame -> Environment
extend env f = f : env

collectConstrain :: Int -> Expr -> Environment -> (Int, LType, Constrains)
collectConstrain cnt (Number _) _  = (cnt, TyInt, [])
collectConstrain cnt (Boolean _) _ = (cnt, TyBool, [])
collectConstrain cnt (Var name) env = (cnt, (lookup env name), [])
collectConstrain cnt (If e1 e2 e3) env =
    let (cnt0, ty1, cs1) = collectConstrain cnt e1 env 
        (cnt1, ty2, cs2) = collectConstrain cnt0 e2 env
                           (cnt2, ty3, cs3) = collectConstrain cnt1 e3 env
     in (cnt2, ty2, [(ty1, TyBool), (ty2, ty3)] ++ cs1 ++ cs2 ++ cs3)

collectConstrain cnt (Let binds body) env = 
    let (cnt', ts, cs) = checkFromEs cnt (snd $ unzip binds)
        env' = extend env (Map.fromList $ zip (fst $ unzip binds) ts)
     in collectConstrain cnt' body env'

collectConstrain cnt (Lambda ps body) env = (cnt'', TyFunc ts tbody, cs)
    where ts = [TVar c| c <- [cnt .. cnt']
          cnt' = cnt + length ps
          env' = extend env (Map.fromList $ zip ps ts)
          (cnt'', tbody, cs) = collectConstrain cnt' body env'

collectConstrain cnt (App f args) env = 
    let (cnt', (tf: targs), cs) = collectFromEs cnt (f:args) env
        te = TVar cnt'
    in (cnt'+1, te, (tf, TyFunc targs te) : cs)

collectConstrain cnt (BPrim op e1 e2) env = 
    case op of 
      Add -> check TyInt TyInt
      Sub -> check TyInt TyInt
      Mul -> check TyInt TyInt
      Div -> check TyInt TyInt
      Mod -> check TyInt TyInt
      Equal -> check TyInt TyBool
      LT -> check TyInt TyBool
      GT -> check TyInt TyBool
      LE -> check TyInt TyBool
      GE -> check TyInt TyBool
      And -> check TyBool TyBool
      Or -> check TyBool TyBool
      where check tyRand tyAns =
          let (cnt', [t1, t2] , cs) = collectFromEs cnt [e1,e2] env
           in (cnt',tyAns, [(t1, tyRand), (t2, tyRand)] ++ cs)

collectConstrain cnt (UPrim op e) env = 
    case op of 
      Not -> let (cnt', t, cs) = collectConstrain cnt e env
              in (cnt', TyBool, (t,  TyBool):cs)



collectFromEs :: Int -> [Expr] -> Environment -> (Int, [LType], Constrains)
collectEs cnt [] _ = (cnt, [], [])
collectFromEs cnt (e: es) env =
    let (cnt', ty, cs) = collectConstrain cnt e env
        (cnt'', ts, cs') = collectFromEs cnt' es env
    in (cnt'', ty:ts, cs ++ cs')


type Subst = [(Int, LType)]
solveConstrain :: Constrains Subst -> Subst
solveConstrain [] s = s
solveConstrain (c:cs) sts = 
    case c of 
      (TyInt, TyInt) -> solveConstrain cs sts
      (TyBool, TyBool) -> solveConstrain cs sts
      ((TyFunc tArgs tAns), (TyFunc tArgs' tAns')) -> solveConstrain ((tAns, tAns'):(zip tArgs tArgs') ++ cs) sts
      ((TVar i), ty) -> solveConstrain (map (replace i ty) cs) ((i, ty):sts)
      (ty, (TVar i)) -> solveConstrain (map (replace i ty) cs) ((i, ty):sts)
      (_, _) -> error "contradiction in constrain"
          where replace i ty0 tyv@(TVar j) = if i == j then ty0 else tyv
                replace i ty0 (TyFunc tArgs tAns) = TyFunc (map (replace i ty0) tArgs) (replace i ty0 tAns)
                replace i ty0 ty = ty


infer :: Expr -> LType
infer expr = let (_, ty, cs) = collectConstrain 0 expr []
                 sts = solveConstrain cs []
              in applySubst ty sts
             where applySubst TyBool _ = TyBool
                   applySubst TyInt _ = TyInt
                   applySubst (TyFunc tArgs tAns) = TyFunc (map (`applySubst` sts) tArgs) (applySubst tAns sts)
                   applySubst (TVar i) ((j, t):rest) = if i == j then t else applySubst (TVar i) rest
                   applySubst _ [] = error "no substitution found"
                   
