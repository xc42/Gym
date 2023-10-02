module TypeInfer where

import LangCore as LC

import qualified Data.Map as Map
import Text.Printf

type Frame          = Map.Map String LC.Type
type Environment    = [Frame]
type Constrains     = [(LC.Type, LC.Type)]

lookupEnv :: Environment  -> String -> LC.Type
lookupEnv [] name          = error $ printf "no found for var : %s" name
lookupEnv (frame:rest) name  = Map.findWithDefault (lookupEnv rest name) name frame

extend :: Environment  -> Frame -> Environment 
extend env f = f : env

collectConstrain :: Int -> Expr -> Environment  -> (Int, LC.Type, Constrains)
collectConstrain cnt (Number _) _  = (cnt, TyInt, [])
collectConstrain cnt (Boolean _) _ = (cnt, TyBool, [])
collectConstrain cnt (Var name) env = (cnt, lookupEnv env name, [])
collectConstrain cnt (If e1 e2 e3) env =
    let (cnt0, ty1, cs1) = collectConstrain cnt e1 env 
        (cnt1, ty2, cs2) = collectConstrain cnt0 e2 env
        (cnt2, ty3, cs3) = collectConstrain cnt1 e3 env
     in (cnt2, ty2, [(ty1, TyBool), (ty2, ty3)] ++ cs1 ++ cs2 ++ cs3)

collectConstrain cnt (Let binds body) env = 
    let (cnt', ts, cs) = collectFromEs cnt (map snd binds) env
        env' = extend env (Map.fromList $ zip (map fst binds) ts)
     in collectConstrain cnt' body env'

collectConstrain cnt (Lambda ps body) env =
    (cnt'', TyFunc ts tbody, cs)
    where ts = [TVar c| c <- [cnt .. cnt'-1]]
          cnt' = cnt + length ps
          env' = extend env (Map.fromList $ zip ps ts)
          (cnt'', tbody, cs) = collectConstrain cnt' body env'

collectConstrain cnt (App f args) env = 
    let (cnt', tf: targs, cs) = collectFromEs cnt (f:args) env
        te = TVar cnt'
    in (cnt'+1, te, (tf, TyFunc targs te) : cs)

collectConstrain cnt (BPrim op e1 e2) env = 
    case op of 
      LC.Add -> check TyInt TyInt
      LC.Sub -> check TyInt TyInt
      LC.Mul -> check TyInt TyInt
      LC.Div -> check TyInt TyInt
      LC.Mod -> check TyInt TyInt
      LC.Equal -> check TyInt TyBool
      LC.LT -> check TyInt TyBool
      LC.GT -> check TyInt TyBool
      LC.LE -> check TyInt TyBool
      LC.GE -> check TyInt TyBool
      LC.And -> check TyBool TyBool
      LC.Or -> check TyBool TyBool
      where
      check tyRand tyAns = let (cnt', [t1, t2] , cs) = collectFromEs cnt [e1,e2] env
        in (cnt',tyAns, [(t1, tyRand), (t2, tyRand)] ++ cs)

collectConstrain cnt (UPrim op e) env = 
    case op of 
      Not -> let (cnt', t, cs) = collectConstrain cnt e env
              in (cnt', TyBool, (t,  TyBool):cs)



collectFromEs :: Int -> [Expr] -> Environment -> (Int, [LC.Type], Constrains)
collectEs cnt [] _ = (cnt, [], [])
collectFromEs cnt (e: es) env =
    let (cnt', ty, cs) = collectConstrain cnt e env
        (cnt'', ts, cs') = collectFromEs cnt' es env
    in (cnt'', ty:ts, cs ++ cs')


type Subst = [(Int, LC.Type)]
solveConstrain :: Constrains -> Subst -> Subst
solveConstrain [] s = s
solveConstrain (c:cs) sts = 
    case c of 
      (TyInt, TyInt) -> solveConstrain cs sts
      (TyBool, TyBool) -> solveConstrain cs sts
      (TyFunc tArgs tAns, TyFunc tArgs' tAns') -> solveConstrain ((tAns, tAns'):zip tArgs tArgs' ++ cs) sts
      (TVar i, ty) -> solveConstrain [(replace i ty tyi, replace i ty tyj)| (tyi, tyj) <- cs]  ((i, ty):sts)
      (ty, TVar i) -> solveConstrain [(replace i ty tyi, replace i ty tyj)| (tyi, tyj) <- cs]  ((i, ty):sts)
      (_, _) -> error "contradiction in constrain"
    where replace i ty0 tyv@(TVar j) = if i == j then ty0 else tyv
          replace i ty0 (TyFunc tArgs tAns) = TyFunc (map (replace i ty0) tArgs) (replace i ty0 tAns)
          replace i ty0 ty = ty


infer :: Expr -> LC.Type
infer expr = let (_, ty, cs) = collectConstrain 0 expr []
                 sts = solveConstrain cs []
              in applySubst ty sts
             where applySubst TyBool _ = TyBool
                   applySubst TyInt _ = TyInt
                   applySubst (TyFunc tArgs tAns) sts = TyFunc (map (`applySubst` sts) tArgs) (applySubst tAns sts)
                   applySubst (TVar i) ((j, t):rest) = if i == j then t else applySubst (TVar i) rest
                   applySubst ty [] = ty
                   
