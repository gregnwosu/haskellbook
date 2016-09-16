{-# LANGUAGE InstanceSigs #-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Identity a =
    Identity { runIdentity :: a }
    deriving (Eq, Show)

newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance (Functor m ) => Functor (IdentityT m) where
    fmap f (IdentityT fa ) = IdentityT $  f <$> fa

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a

instance Applicative m => Applicative (IdentityT m) where
    pure  = IdentityT . pure
    (IdentityT fab ) <*> (IdentityT fa) = IdentityT (fab <*> fa )

instance (Monad m) => Monad (IdentityT m ) where
    return = pure
    (>>=) :: IdentityT m a
          -> (a -> IdentityT m b)
          -> IdentityT m b
    (IdentityT ma) >>= f =
        let
            aimb = ma >>= (runIdentityT .  f) 
        in IdentityT aimb


           
newtype MaybeT m a =
    MaybeT {runMaybeT :: m (Maybe a)}


instance (Functor m ) => Functor (MaybeT m) where
    fmap f (MaybeT ma) =
        MaybeT $ (fmap . fmap ) f ma

-- instance (Applicative m) => Applicative (MaybeT m) where
--     pure x = MaybeT . pure . pure $ x
--     (MaybeT fab) <*> (MaybeT mma) = MaybeT $ fab <*> mma



innerMost :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

second' :: [Maybe (Identity a -> Identity b)] ->
          [Maybe (Identity a) -> Maybe (Identity b)]
second' = fmap (<*>)

final' :: [Maybe (Identity a) -> Maybe (Identity b)] ->
         [Maybe (Identity a)] -> [Maybe (Identity b)]
final' = (<*>)


lmiApply :: [Maybe (Identity (a -> b))]
           -> [Maybe (Identity a)]
           -> [Maybe (Identity b)]
lmiApply f x =
    final' (second' (innerMost f)) x


instance (Applicative m) => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))
    (MaybeT fab) <*> (MaybeT mma) =
        MaybeT $ (<*>) <$> fab <*> mma



newtype ReaderT r m a =
    RunReaderT { runReaderT :: r -> m a}