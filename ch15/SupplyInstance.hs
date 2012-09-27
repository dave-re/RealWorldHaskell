{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad

newtype Reader e a = R { runReader :: e -> a }
newtype MySupply e a = MySupply { runMySupply :: Reader e a }
    deriving (Monad)

instance MonadSupply e (MySupply e) where
    next = MySupply $ do
            v <- ask
            return (Just v)

    -- more concise:
    -- next = MySupply (Just `liftM` ask)

ask :: Reader e e
ask = R id