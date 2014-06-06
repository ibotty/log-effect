{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Control.Eff.Log
  ( Log(Log, logLine)
  , Logger
  , logE
  , filterLog
  , filterLog'
  , runLogPure
  , runLogStdout
  , runLogStderr
  , runLogFile
  , runLogWithLoggerSet
  , runLog
  ) where

import Control.Applicative   ((<$>), (<*))
import Control.Eff           ((:>), Eff, Member, SetMember, VE (..), admin,
                              handleRelay, inj, interpose, send)
import Control.Eff.Lift      (Lift, lift)
import Data.Monoid           ((<>))
import Data.Typeable         (Typeable, Typeable1)
import System.Log.FastLogger (LogStr, LoggerSet, ToLogStr, defaultBufSize,
                              flushLogStr, fromLogStr, newFileLoggerSet,
                              newStderrLoggerSet, newStdoutLoggerSet,
                              pushLogStr, toLogStr)

import qualified Data.ByteString.Char8 as B8

data Log a v = Log
  { logLine :: a
  , logNext :: v
  } deriving (Typeable, Functor)

-- | a monadic action that does the real logging
type Logger m l = forall v. Log l v -> m ()

-- | Log something.
logE :: (Typeable l, Member (Log l) r)
  => l -> Eff r ()
logE line = send $ \next -> inj (Log line (next ()))

-- | Collect log messages in a list.
runLogPure :: (Typeable l)
  => Eff (Log l :> r) a
  -> Eff r (a, [l])
runLogPure = go . admin
  where go (Val v) = return (v, [])
        go (E req) = handleRelay req go performLog
        performLog l = fmap (prefixLogWith l) (go (logNext l))
        prefixLogWith log' (v, l) = (v, logLine log' : l)

-- | Run the 'Logger' action in the base monad for every log line.
runLog :: (Typeable l, Typeable1 m, SetMember Lift (Lift m) r)
  => Logger m l -> Eff (Log l :> r) a -> Eff r a
runLog logger = go . admin
  where go (Val v) = return v
        go (E req) = handleRelay req go performLog
        performLog l = lift (logger l) >> go (logNext l)

-- | Filter Log entries with a predicate.
--
-- Note that, most of the time an explicit type signature for the predicate
-- will be required.
filterLog :: (Typeable l, Member (Log l) r)
  => (l -> Bool) -> Eff r a -> Eff r a
filterLog f = go . admin
  where go (Val v) = return v
        go (E req) = interpose req go filter'
          where filter' (Log l v) = if f l then send (<$> req) >>= go
                                           else go v

-- | Filter Log entries with a predicate and a proxy.
--
-- This is the same as 'filterLog' but with a proxy l for type inference.
filterLog' :: (Typeable l, Member (Log l) r)
  => (l -> Bool) -> proxy l -> Eff r a -> Eff r a
filterLog' predicate _ = filterLog predicate

-- | Log to stdout.
runLogStdout :: (Typeable l, ToLogStr l, SetMember Lift (Lift IO) r)
  => proxy l -> Eff (Log l :> r) a -> Eff r a
runLogStdout proxy eff = do
    s <- lift $ newStdoutLoggerSet defaultBufSize
    runLogWithLoggerSet s proxy eff <* lift (flushLogStr s)

-- | Log to stderr.
runLogStderr :: (Typeable l, ToLogStr l, SetMember Lift (Lift IO) r)
  => proxy l -> Eff (Log l :> r) a -> Eff r a
runLogStderr proxy eff = do
    s <- lift $ newStderrLoggerSet defaultBufSize
    runLogWithLoggerSet s proxy eff <* lift (flushLogStr s)

-- | Log to file.
runLogFile :: (Typeable l, ToLogStr l, SetMember Lift (Lift IO) r)
  => FilePath -> proxy l -> Eff (Log l :> r) a -> Eff r a
runLogFile f proxy eff = do
    s <- lift $ newFileLoggerSet defaultBufSize f
    runLogWithLoggerSet s proxy eff <* lift (flushLogStr s)

-- | Log to a file using a 'LoggerSet'.
--
-- Note, that you will still have to call 'flushLogStr' on the 'LoggerSet'
-- at one point.
--
-- With that function you can combine a logger in a surrounding IO action
-- with a logger in the 'Eff' effect.
--
-- >data Proxy a = Proxy
-- >
-- > main :: IO ()
-- > main = do
-- >     loggerSet <- newStderrLoggerSet defaultBufSize
-- >     pushLogStr loggerSet "logging from outer space^WIO\n"
-- >     runLift $ runLogWithLoggerSet loggerSet (Proxy :: Proxy String) $
-- >         logE ("logging from within Eff" :: String)
-- >     flushLogStr loggerSet
runLogWithLoggerSet :: (Typeable l, ToLogStr l, SetMember Lift (Lift IO) r)
  => LoggerSet -> proxy l -> Eff (Log l :> r) a -> Eff r a
runLogWithLoggerSet s _ = runLog (loggerSetLogger s)

loggerSetLogger :: ToLogStr l => LoggerSet -> Logger IO l
loggerSetLogger loggerSet = pushLogStr loggerSet . (<> "\n") . toLogStr . logLine
