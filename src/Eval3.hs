module Eval3
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

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva m\'onada que
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|.
newtype StateErrorTrace a = StateErrorTrace {runStateErrorTrace :: Env -> Either Error (Pair a (Env, Trace))}

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure = return
  (<*>) = ap

instance Monad StateErrorTrace where
  return x = StateErrorTrace (\s -> Right (x :!: (s, "")))
  m >>= f =
    StateErrorTrace
      (\s -> do
        (a :!: (e, t)) <- runStateErrorTrace m s
        (b :!: (e', t')) <- runStateErrorTrace (f a) e
        return $ b :!: (e', t ++ t'))

-- Ejercicio 3.b: Resolver en Monad.hs

-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  addTrace s = StateErrorTrace (\e -> Right ( () :!: (e, s)))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StateErrorTrace (\_ -> Left e)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace $ lookfor' v
    where
      lookfor' v s = maybe (Left UndefVar) (Right . (:!: (s, ""))) $ M.lookup v s
  update v i = StateErrorTrace (\s -> Right (() :!: (M.insert v i s, "")))

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval p = case runStateErrorTrace (stepCommStar p) initEnv of
  Right (_:!:s) -> Right s
  Left x -> Left x

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadTrace m, MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadTrace m, MonadState m, MonadError m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let s exp) = do
  v <- evalExp exp
  update s v
  addTrace ("let " ++ s ++ " = " ++ show v ++ "; ")
  return Skip
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
evalExp :: (MonadTrace m, MonadState m, MonadError m) => Exp a -> m a
evalExp (Const n) = return n
evalExp (Var s) = lookfor s
evalExp (UMinus exp) = negate <$> evalExp exp
evalExp (Plus exp exp') = (+) <$> evalExp exp <*> evalExp exp'
evalExp (Minus exp exp') = (-) <$> evalExp exp <*> evalExp exp'
evalExp (Times exp exp') = (*) <$> evalExp exp <*> evalExp exp'
evalExp (Div exp exp') = do
  r <- evalExp exp
  t <- evalExp exp'
  if t == 0 then throw DivByZero else return (r `div` t)
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
  addTrace ("let " ++ s ++ " = " ++ show r ++ "; ")
  return r
evalExp (ESeq exp exp') = evalExp exp >> evalExp exp'
