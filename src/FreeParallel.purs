module Free.Parallel where

import Prelude
import Control.Applicative.Free (FreeAp, hoistFreeAp, liftFreeAp, retractFreeAp)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Newtype (class Newtype, unwrap, wrap)
import Free (Free(..), applyInner, foldFree, hoistFree, liftF)

newtype FreeParallel f a = FreePar (Free (FreeAp f) a)

derive instance newtypeFreePar :: Newtype (FreeParallel f a) _

instance functorFreePar :: Functor f => Functor (FreeParallel f) where
  map f (FreePar fp) = FreePar $ map f fp

instance applyFreePar :: Functor f => Apply (FreeParallel f) where
  apply (FreePar ff) (FreePar fa) = FreePar $ apply ff fa

instance applicativeFreePar :: Functor f => Applicative (FreeParallel f) where
  pure = Pure >>> FreePar

instance bindFreePar :: Functor f => Bind (FreeParallel f) where
  bind (FreePar fa) k = FreePar $ fa >>= (\a -> unwrap $ k a) 

instance monadFreePar :: Functor f => Monad (FreeParallel f)

parRunAp :: forall m f. Parallel f m => FreeAp m ~> m
parRunAp fma = 
  let ffa = hoistFreeAp parallel fma
      fa = retractFreeAp ffa
  in sequential fa


retractFreePar :: forall m f a. Parallel f m => FreeParallel m a -> m a
retractFreePar (FreePar fpma) =
  foldFree parRunAp fpma


foldFreePar :: forall f m g a. Parallel f m => (g ~> m) -> FreeParallel g a -> m a
foldFreePar f (FreePar fpfa) =
  foldFree ((hoistFreeAp f) >>> parRunAp) fpfa
  
   
hoistFreePar :: forall f g a. Functor g => (f ~> g) -> FreeParallel f a -> FreeParallel g a
hoistFreePar f (FreePar fpfa) = FreePar $ 
  hoistFree (hoistFreeAp f) fpfa

liftFreePar :: forall f a. f a -> FreeParallel f a
liftFreePar fa = FreePar $ liftF $ liftFreeAp fa

newtype ParFreeParallel f a = ParFreePar (FreeParallel f a)

derive instance newtypeParFreePar :: Newtype (ParFreeParallel f a) _

instance functorParFreePar :: Functor f => Functor (ParFreeParallel f) where
  map f (ParFreePar fp) = ParFreePar $ map f fp

instance applyParFreePar :: Functor f => Apply (ParFreeParallel f) where
  apply (ParFreePar (FreePar ff)) (ParFreePar (FreePar fa)) = 
    ParFreePar $ FreePar $ applyInner ff fa

instance applicativeParFreePar :: Functor f => Applicative (ParFreeParallel f) where
  pure = pure >>> ParFreePar

instance parallelFreePar :: Functor f => Parallel (ParFreeParallel f) (FreeParallel f) where
  sequential = unwrap
  parallel = wrap