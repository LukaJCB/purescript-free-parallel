module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Parallel (class Parallel, parSequence, sequential)
import Control.Parallel.Class (parallel)
import Data.Either (Either(..))
import Data.Traversable (sequence)
import Data.Tuple.Nested (Tuple3, tuple3)
import Free.Parallel (FreeParallel, foldFreePar, liftFreePar)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow (foldFreePar interp program)
  logShow (foldFreePar interp parProgram)


parApply
  :: forall f m a b
   . Parallel f m
   => m (a -> b)
   -> m a
   -> m b
parApply mf ma = sequential(apply (parallel mf) (parallel ma))
infixl 4 apply as <!>
data Alg a = Print String a | Read (String -> a) | PrintErr String a

instance functorAlg :: Functor Alg where
  map f alg = case alg of
    (Print s a) -> Print s (f a)
    (Read g) -> Read (map f g)
    (PrintErr s a) -> PrintErr s (f a)

type Console a = FreeParallel Alg a

printLn :: String -> Console Unit
printLn s = liftFreePar (Print s unit)

printErr :: String -> Console Unit
printErr s = liftFreePar (PrintErr s unit)

readLn :: Console String
readLn = liftFreePar (Read id)

interp :: Alg ~> Either String 
interp c = case c of
  (Print s a) -> Right a
  (PrintErr s a) -> Left s
  (Read k) -> pure (k "Hey guys")

program :: FreeParallel Alg String
program = do
  a <- readLn
  printLn a
  xs <- sequence [printLn a, printErr "WTF", printErr "Yeah"]
  b <- readLn
  pure $ a <> b

parProgram :: FreeParallel Alg String
parProgram = do
  a <- readLn
  printLn a
  xs <- parSequence [printLn a, printErr "WTF", printErr "Yeah"]
  b <- readLn
  pure $ a <> b
 
