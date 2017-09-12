module Free where

import Prelude

data Free f a = Pure a 
  | Free (f (Free f a))

instance functorFree :: Functor f => Functor (Free f) where
  map f free = case free of 
    (Pure a) -> Pure (f a)
    (Free a) -> Free (map (map f) a)

instance applyFree :: Functor f => Apply (Free f) where
  apply ff fa = case ff of
    (Pure f) -> map f fa
    (Free f) -> Free $ map (_ <*> fa) f

instance applicativeFree :: Functor f => Applicative (Free f) where
  pure = Pure

instance bindFree :: Functor f => Bind (Free f) where
  bind fa k = case fa of
    (Pure a) -> k a
    (Free a) -> Free $ map (_ >>= k) a

instance monadFree :: Functor f => Monad (Free f)

hoistFree :: forall f g a. Functor g => (f ~> g) -> Free f a -> Free g a
hoistFree _ (Pure a)  = Pure a
hoistFree f (Free as) = Free (hoistFree f <$> f as)

foldFree :: forall f m a. Monad m => (f ~> m) -> Free f a -> m a
foldFree _ (Pure a)  = pure a
foldFree f (Free as) = f as >>= foldFree f

retractFree :: forall f a. Monad f => Free f a -> f a
retractFree (Pure a) = pure a
retractFree (Free as) = as >>= retractFree

liftF :: forall f a. Functor f => f a -> Free f a
liftF fa = Free (map Pure fa)

applyInner :: forall f a b. Applicative f => Free f (a -> b) -> Free f a -> Free f b
applyInner (Pure a) (Pure b) = Pure $ a b
applyInner (Pure a) (Free mb) = Free $ map a <$> mb
applyInner (Free ma) (Pure b) = Free $ map (_ $ b) <$> ma
applyInner (Free ma) (Free mb) = Free $ map (<*>) ma <*> mb