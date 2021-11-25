module Eval1
  ( eval,
    Env,
  )
where

import AST
import Control.Monad
  ( ap,
    liftM,
  )
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Strict.Tuple
import Monads
import Prelude hiding
  ( fst,
    snd,
  )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State {runState :: Env -> Pair a Env}

instance Monad State where
  return x = State (x :!:)
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> lookfor' v s :!: s)
    where
      lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> () :!: update' v i s) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let s exp) = evalExp exp >>= update s >> return Skip
stepComm (Seq Skip co') = return co'
stepComm (Seq co co') = do
  r <- stepComm co
  return $ Seq r co'
stepComm (IfThenElse exp co co') = do
  r <- evalExp exp
  return $ if r then co else co'
stepComm c@(While exp co) = do
  r <- evalExp exp
  return $ if r then Seq co c else Skip

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const n) = return n
evalExp (Var s) = lookfor s
evalExp (UMinus exp) = negate <$> evalExp exp
evalExp (Plus exp exp') = (+) <$> evalExp exp <*> evalExp exp'
evalExp (Minus exp exp') = (-) <$> evalExp exp <*> evalExp exp'
evalExp (Times exp exp') = (*) <$> evalExp exp <*> evalExp exp'
evalExp (Div exp exp') = div <$> evalExp exp <*> evalExp exp'
evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt exp exp') = (<) <$> evalExp exp <*> evalExp exp'
evalExp (Gt exp exp') = (>) <$> evalExp exp <*> evalExp exp'
evalExp (And exp exp') = (&&) <$> evalExp exp <*> evalExp exp'
evalExp (Or exp exp') = (||) <$> evalExp exp <*> evalExp exp'
evalExp (Not exp) = not <$> evalExp exp
evalExp (Eq exp exp') = (==) <$> evalExp exp <*> evalExp exp'
evalExp (NEq exp exp') = (/=) <$> evalExp exp <*> evalExp exp'
evalExp (EAssgn s exp) = do
  r <- evalExp exp
  update s r
  return r
evalExp (ESeq exp exp') = evalExp exp >> evalExp exp'
