module Either.Parallel where
  
import Prelude
import Control.Parallel (class Parallel)
import Control.Parallel.Class (sequential)
import Data.Either (Either(..), either)
import Data.Validation.Semigroup (V, invalid, unV)


{-
instance parallelEitherV :: Semigroup e => Parallel (V e) (Either e) where
  sequential = unV Left Right
  parallel = either invalid pure
-}