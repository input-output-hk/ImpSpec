{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.ImpSpec.Repl where

import Control.Monad.Reader
import Data.IORef
import Data.Proxy
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)
import Test.ImpSpec
import Test.QuickCheck.Random (mkQCGen)

data AnyImpEnv where
  NoImpEnv :: AnyImpEnv
  AnyImpEnv :: Typeable t => ImpEnv t -> AnyImpEnv

replEnv :: IORef AnyImpEnv
replEnv = unsafePerformIO $ newIORef NoImpEnv
{-# NOINLINE replEnv #-}

impResetReplEnv :: IO ()
impResetReplEnv = writeIORef replEnv NoImpEnv

impInitReplEnv :: forall t. (ImpSpec t, Typeable t) => Proxy t -> IO ()
impInitReplEnv _ = do
  let qcGen = mkQCGen 2024
  impInit :: ImpInit t <- impInitIO qcGen
  impEnv <- newImpEnv (Just qcGen) Nothing impInit
  writeIORef replEnv $ AnyImpEnv impEnv
  imp $ impPrepAction @t

-- | Execute an `ImpM` action in a repl. It will prepare the necessary state and environment using
-- `ImpSpec` type class. In order to reset the environment call `impResetReplEnv`
--
-- >>> data I; instance ImpSpec I
-- >>> imp (liftIO (print "foo") :: ImpM I ())
-- "foo"
imp :: forall t a. (ImpSpec t, Typeable t) => ImpM t a -> IO a
imp action@(ImpM m) = do
  let t = Proxy @t
  readIORef replEnv >>= \case
    NoImpEnv -> impInitReplEnv t >> imp action
    AnyImpEnv anyEnv -> do
      case cast anyEnv :: Maybe (ImpEnv t) of
        Nothing ->
          error $
            "Mismatch of ImpSpec enviroment: '"
              <> show (typeRep t)
              <> "', currently working in: '"
              <> show (typeRep anyEnv)
              <> "'. Need to reset with 'impResetReplEnv'"
        Just env -> do
          res <- runReaderT m env
          writeIORef replEnv $ AnyImpEnv env
          pure res

-- | In case you like using repl commands:
--
-- >>> data I; instance ImpSpec I
-- >>> :def imp repl "I"
-- >>> :imp liftIO (print "foo") :: ImpM I ()
-- "foo"
repl :: String -> String -> IO String
repl t fullExpr =
  let wrapExpr e = "imp @(" <> t <> ") (" <> e <> ")"
   in pure $ case words fullExpr of
        bind : "<-" : bodyWords ->
          bind <> " <- " <> wrapExpr (unwords bodyWords)
        _ -> wrapExpr fullExpr
