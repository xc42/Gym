module TypeInfer where

import LangCore as LC
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Text.Printf

type Frame          = Map.Map String LC.Type
type Environment    = [Frame]
type Constrains     = [(LC.Type, LC.Type)]

lookupEnv :: Environment  -> String -> LC.Type
lookupEnv [] name          = error $ printf "no found for var : %s" name
lookupEnv (frame:rest) name  = Map.findWithDefault (lookupEnv rest name) name frame

extend :: Environment  -> Frame -> Environment 
extend env f = f : env

newTVar :: State Int LC.Type
newTVar = state (\cnt-> (TVar cnt, cnt+1))

generalize :: LC.Type -> Int -> LC.Type
generalize TyBool _ = TyBool
generalize TyInt _  = TyInt
generalize (TVar v) t0 =  if v < t0 then TVar v else QVar v
generalize (QVar v) _ = QVar v
generalize (TyFunc args rty) t0 = TyFunc args' rty'
    where rty':args' = map (`generalize` t0) (rty:args)


instanitiate :: LC.Type -> State Int LC.Type
instanitiate TyBool = return TyBool
instanitiate TyInt  = return TyInt
instanitiate (TVar v) = return $ TVar v
instanitiate (QVar v) = newTVar
instanitiate tyFn@(TyFunc args rty) =
    let qvs = scanQ IntSet.empty tyFn
        nq  = IntSet.size qvs
    in do 
        tvs <- replicateM nq newTVar
        let imap = IntMap.fromList $ zip (IntSet.toList qvs) tvs
        return $ instQ imap tyFn
    where scanQ acc (QVar v) = IntSet.insert v acc
          scanQ acc (TyFunc args rty) = foldl scanQ acc (rty:args)
          scanQ acc _ = IntSet.empty
          instQ imap (QVar v) = IntMap.findWithDefault (TVar (-1)) v imap
          instQ imap (TyFunc args rty) = TyFunc (map (instQ imap) args) (instQ imap rty)
          instQ imap ty = ty


collectConstrain :: Expr -> Environment  -> State Int (LC.Type, Constrains)
collectConstrain (Number _) _  = return (TyInt, [])
collectConstrain (Boolean _) _ = return (TyBool, [])
collectConstrain (Var name) env = do { ity <- instanitiate $ lookupEnv env name; return (ity, []) }
collectConstrain (If e1 e2 e3) env = do 
    (ty1, cs1) <- collectConstrain e1 env
    (ty2, cs2) <- collectConstrain e2 env
    (ty3, cs3) <- collectConstrain e3 env
    return (ty2, [(ty1, TyBool), (ty2, ty3)] ++ cs1 ++ cs2 ++ cs3)

collectConstrain (Let binds body) env = do
    tsAndcs <- mapM ((`collectConstrain` env) . snd) binds
    t0 <- get
    let ts   = map ((`generalize` t0) . fst) tsAndcs
    let css  = map snd tsAndcs
    let env' = extend env (Map.fromList $ zip (map fst binds) ts)
    (tbody, cs') <- collectConstrain body env'
    return (tbody, foldr (++) cs' css)

collectConstrain (Lambda ps body) env = do
    tvs <- replicateM (length ps) newTVar
    let env' = extend env (Map.fromList $ zip ps tvs)
    (tbody, cs) <- collectConstrain body env'
    return (TyFunc tvs tbody, cs)


collectConstrain (App f args) env = do
    (fty, fcs) <- collectConstrain f env
    argsTsCs <- mapM (`collectConstrain` env) args
    te <- newTVar
    return (te, (fty, TyFunc (map fst argsTsCs) te) : foldr ((++) . snd) fcs argsTsCs)

collectConstrain (BPrim op e1 e2) env = 
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
          check tyRand tyAns = do
            (ty1, cs1) <- collectConstrain e1 env
            (ty2, cs2) <- collectConstrain e2 env
            return (tyAns, [(ty1, tyRand), (ty2, tyRand)] ++ cs1 ++ cs2)

collectConstrain (UPrim op e) env = 
    case op of 
      Not -> do { (ty, cs) <- collectConstrain e env; return (TyBool, (ty, TyBool): cs) }


type Subst = [(Int, LC.Type)]
solveConstrain :: Constrains -> Subst -> Subst
solveConstrain [] s = s
solveConstrain (c:cs) sts = 
    case c of 
      (TyInt, TyInt) -> solveConstrain cs sts
      (TyBool, TyBool) -> solveConstrain cs sts
      (TyFunc tArgs tAns, TyFunc tArgs' tAns') -> solveConstrain ((tAns, tAns'):zip tArgs tArgs' ++ cs) sts
      (TVar i, ty) -> solveTVar i ty
      (ty, TVar i) -> solveTVar i ty
      (_, _) -> error "contradiction in constrain"
    where solveTVar i ty = solveConstrain [(replace i ty tyi, replace i ty tyj)| (tyi, tyj) <- cs]
                                          ((i, ty):sts)
          replace i ty0 tyv@(TVar j) = if i == j then ty0 else tyv
          replace i ty0 (TyFunc tArgs tAns) = TyFunc (map (replace i ty0) tArgs) (replace i ty0 tAns)
          replace i ty0 ty = ty


infer :: Expr -> LC.Type
infer expr = let ((ty, cs), _) = runState (collectConstrain expr []) 0
                 sts = solveConstrain cs []
                 applySubst TyBool _ = TyBool
                 applySubst TyInt _ = TyInt
                 applySubst (TyFunc tArgs tAns) sts = TyFunc (map (`applySubst` sts) tArgs) (applySubst tAns sts)
                 applySubst ty@(TVar i) ((j, t):rest) = if i == j then applySubst t sts else applySubst ty rest
                 applySubst ty [] = ty
              in 
                applySubst ty sts
