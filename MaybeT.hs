module MaybeT where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Maybe

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
  lift m = MaybeT (liftM Just m)

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT (fmap (fmap f) m)

instance Applicative m => Applicative (MaybeT m) where
  MaybeT a <*> MaybeT b = MaybeT $ (<*>) <$> a <*> b
  pure = MaybeT . pure . Just

instance Monad m => Monad (MaybeT m) where
  m >>= f  = MaybeT $ runMaybeT m >>= maybe (return Nothing) (runMaybeT . f)
  return x = MaybeT (return (Just x))
  fail x   = MaybeT (return Nothing)

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = MaybeT . liftM Just . liftIO

fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT x = fmap (fromMaybe x) . runMaybeT
