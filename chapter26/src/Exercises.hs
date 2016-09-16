module Exercises where

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a)}

instance Functor m =>
    Functor (EitherT e m) where
    fmap f (EitherT mea) =
        EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
    pure x = EitherT (pure (pure x))
    (EitherT mef) <*> (EitherT mea) = EitherT $ (<*>) <$> mef <*> mea

instance Monad m => Monad (EitherT e m ) where
    return = pure
    (EitherT mea) >>= fa2Emeb =
        EitherT $ mea >>=
          either (pure . Left) (runEitherT . fa2Emeb)

swapEitherT :: (Functor m ) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT  meac) = EitherT $ either Right Left  <$> meac



eitherT :: Monad m =>
          (a -> m c)
          -> (b -> m c)
          -> EitherT a m b
          -> m c
eitherT fa fb (EitherT mab) = mab >>= either fa fb
