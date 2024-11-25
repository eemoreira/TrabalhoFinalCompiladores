module ErrorMonad where
data M a = MS(String, a) deriving Show

instance Functor M where
  fmap :: (a -> b) -> M a -> M b
  fmap f(MS (s, a)) = MS(s, f a)

instance Applicative M where
  pure :: a -> M a
  pure a = MS("", a)
  (<*>) :: M (a -> b) -> M a -> M b
  MS(s, f) <*> MS(t, x) = MS(s ++ t, f x)

instance Monad M where
  (>>=) :: M a -> (a -> M b) -> M b
  MS m >>= f = 
    let (s, a) = m in
    let MS(t, b) = f a in
    MS(s ++ t, b)

