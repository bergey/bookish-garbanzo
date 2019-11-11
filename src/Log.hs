{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}

-- | Structured logging with adjustable verbosity & IO-free logs during testing.

module Log where

import           Control.Arrow (second)
import           Control.Monad (when)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.RWS.Lazy
import           Data.Aeson hiding (Error)
import           Data.Aeson.TH
import           Data.Data
import           Data.HashMap.Lazy (HashMap)
import           Data.Semigroup
import           Data.Text (Text)
import           GHC.Stack
import           Prelude hiding (log)

import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BSL (putStrLn)
import qualified Data.HashMap.Lazy as HM

data Level
    = Debug -- ^ Useful when developing new feature, or tracking a bug
    | Info -- ^ Useful for monitoring that process is running normally
    | Warning -- ^ HTTP 4xx, code is not succeeding, but caller may recover
    | Error -- ^ HTTP 5xx, unrecoverable error
  deriving (Eq, Ord, Bounded, Show, Read, Typeable, Data)
$(deriveJSON defaultOptions ''Level)

type Message = HashMap Text JSON.Value
type Pair = JSON.Pair

class Monad m => Log (m :: * -> *) where
    minLevel :: m Level
    context :: m Message
    withContext' :: Message -> m a -> m a
    unconditionally :: Message -> m ()

formatCallSite :: (String, SrcLoc) -> Message
formatCallSite (functionName, srcLoc) = Log.fromList [
    "filePath" .= srcLocFile srcLoc
    , "lineNumber" .= srcLocStartLine srcLoc
    , "functionName" .= functionName
    ]

-- | This is the most general way to log, implementing the usual logic
-- of context and log levels.
log :: (HasCallStack, Log m) => Level -> HashMap Text JSON.Value -> m ()
log level msg = do
    threshold <- minLevel
    cxt <- context
    let
        -- skip this function, report where it was called
        stack = getCallStack callStack
        reportLocation = case stack of
            [] -> Nothing
            [c] -> Just (formatCallSite c)
            -- line where 'error' was called, function we were in
            (c1:c2:_) -> Just (formatCallSite (fst c2, snd c1))
        stackDriverContext = HM.fromList [
            "severity" .= level
            -- TODO distinguish service, environment
            , "context" .= Log.fromList [ "reportLocation" .= reportLocation ]
            , "callStack" .= map formatCallSite stack
            ]
        combined = stackDriverContext <> msg <> cxt
    when (level >= threshold) $ unconditionally combined

error :: (HasCallStack, Log m) => [(Text, JSON.Value)] -> m ()
error msg = withFrozenCallStack (log Error (HM.fromList msg))

warn :: (HasCallStack, Log m) => [(Text, JSON.Value)] -> m ()
warn msg = withFrozenCallStack (log Warning (HM.fromList msg))

info :: (HasCallStack, Log m) => [(Text, JSON.Value)] -> m ()
info msg = withFrozenCallStack (log Info (HM.fromList msg))

debug :: (HasCallStack, Log m) => [(Text, JSON.Value)] -> m ()
debug msg = withFrozenCallStack (log Debug (HM.fromList msg))

withContext :: Log m => [(Text, Value)] -> m a -> m a
withContext c = withContext' (fromList c)

fromList :: [(Text, JSON.Value)] -> Message
fromList = HM.fromList
--------------------------------------------------------------------------------
-- Instances

-- | Suitable for running tests, where logging is the only effect in
-- the function under test.
instance Log (RWS (Level, Message) [Message] ()) where
  minLevel = asks fst
  context = asks snd
  withContext' c = local (second (c <>))
  unconditionally m = tell [m]

newtype NoLog m a = NoLog (m a)
    deriving (Functor, Applicative, Monad)

instance Monad m => Log (NoLog m) where
    minLevel = NoLog (return Error)
    context = NoLog (return mempty)
    withContext' _ m = m
    unconditionally _ = NoLog (return ())

instance Log m => Log (ExceptT e m) where
    minLevel = lift minLevel
    context = lift context
    withContext' c m = ExceptT (withContext' c (runExceptT m))
    unconditionally = lift . unconditionally

-- | This is handy for logging in main before the context is set up.
logIO :: Level -> [(Text, JSON.Value)] -> IO ()
logIO level fields = BSL.putStrLn . encode . HM.fromList $ ["severity" .= level ] <> fields

withRight :: (ToJSON e, Log m) => Either e a -> (a -> m ()) -> m ()
withRight (Left e) _ = Log.error [ "error" .= e ]
withRight (Right a) f = f a

withRightT :: (ToJSON e, Log m) => ExceptT e m () -> m ()
withRightT eem = runExceptT eem >>= flip withRight pure

left :: (ToJSON e, Log m) => Either e a -> m ()
left eea = withRight eea (\_ -> return ())
