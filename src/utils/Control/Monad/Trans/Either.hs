{-# LANGUAGE InstanceSigs #-}
module Control.Monad.Trans.Either 
(
    EitherT (..),
    lift,
    liftEither
)
where


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap :: Functor m => (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f = EitherT . ((f <$>) <$>) . runEitherT

instance Applicative m => Applicative (EitherT e m) where
    pure :: Applicative m => a -> EitherT e m a
    pure = EitherT . pure . Right
    (<*>) :: Applicative m => EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    (EitherT f) <*> (EitherT a) = EitherT $ liftA2 (<*>) f a

instance Monad m => Monad (EitherT e m) where
    (>>=) :: Monad m => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    m >>= k = EitherT $ do
        m' <- runEitherT m
        case m' of
            Left l -> pure $ Left l
            Right m'' -> runEitherT (k m'')


lift :: (Functor m) => m a -> EitherT e m a
lift = EitherT . (Right <$>) 

liftEither :: (Applicative m) => Either e a -> EitherT e m a
liftEither = EitherT . pure


-- TODO Misschien ook (of in plaats van dit) een ErrorT (of zonder T, of allebei?) bouwen die fatal/nietfatal errors en oook warnings en ook alles kan opsparen
