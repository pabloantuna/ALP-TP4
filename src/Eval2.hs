module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> runStateError m s >>= \(v :!: s') -> runStateError (f v) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw e = StateError (\s -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> case M.lookup v s of
                                  Nothing -> Left UndefVar
                                  Just x -> Right (x :!: s))
  update v i = StateError (\s -> Right (() :!: update' v i s)) where update' = M.insert


-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = case runStateError (stepCommStar p) initEnv of
  Right (_:!:s) -> Right s
  Left x -> Left x

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
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
  return r
evalExp (ESeq exp exp') = evalExp exp >> evalExp exp'

