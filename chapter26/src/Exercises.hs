{-# LANGUAGE TupleSections #-}
module Exercises where
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.IO.Class
import Lib
 
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

-- but I want to do
swapEitherT' ::  (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT'  =  (go . fmap )  ( either Right Left )
                 where go :: (m (Either e a) -> n (Either e' b)) -> EitherT e m a -> EitherT e' n b
                       go = undefined

eitherT :: Monad m =>
          (a -> m c)
          -> (b -> m c)
          -> EitherT a m b
          -> m c
eitherT fa fb (EitherT mab) = mab >>= either fa fb

instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT fr2ma) = ReaderT $ (fmap . fmap) f fr2ma

instance Applicative m => Applicative (ReaderT r m) where
    pure x = ReaderT $ const (pure x)
    (ReaderT rmf) <*> (ReaderT rma) = ReaderT $  (<*>) <$> rmf <*> rma

instance Monad m => Monad (ReaderT r m) where
    return = pure
    (ReaderT rma) >>= a2RTrmb = ReaderT $ \r ->
                                do
                                  a <- rma r
                                  runReaderT (a2RTrmb a) r

newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s)}

instance (Functor m ) => Functor (StateT s m) where
    fmap f (StateT s2mas) = let go (a, s) = (f a, s)
                            in StateT $ (fmap . fmap ) go s2mas

instance (Monad m) => Applicative (StateT s m) where
    pure x = StateT $ \s -> pure (x,s)
    (StateT s2mfs) <*> (StateT s2mas) =
        StateT $ \s -> do
          (f,s') <- s2mfs s
          (a,s'') <- s2mas s'
          return (f a, s'')

instance (Monad m) => Monad (StateT s m) where
    (StateT s2mas) >>= a2STsmb =
        StateT $ \s -> do
          (a,s')  <- s2mas s
          runStateT (a2STsmb a) s'

newtype RWST r w s m a =
    RWST { runRWST :: r -> s -> m (a, s, w)}

instance MonadTrans (EitherT e ) where
    lift = EitherT . fmap Right

instance MonadTrans (StateT s) where
    lift x = StateT $ \s -> fmap (,s) x

instance MonadTrans MaybeT  where
    lift x = MaybeT $  Just <$> x

instance MonadTrans (ReaderT r) where
    lift x = ReaderT  $ const x

instance (MonadIO m) => MonadIO (IdentityT m) where
    liftIO  = IdentityT . liftIO

instance (MonadIO m) => MonadIO (EitherT e m) where
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT r m) where
    liftIO = lift . liftIO
             