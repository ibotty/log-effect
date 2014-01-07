{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Control.Eff.Log
  ( Log(Log)
  , ShowLog
  , showLog
  , logLine
  , logE
  , filterLog
  , runPureLog
  , runLogStdErr
  , runLog
  ) where

import Control.Applicative ((<$>))
import Control.Eff ( Eff, SetMember, VE(..), (:>)
                   , admin, handleRelay, inj, interpose, send)
import Control.Eff.Lift (Lift, lift)
import Data.Typeable (Typeable, Typeable1)
import System.IO (hPutStrLn, stderr)

data Log a v = Log
  { logLine :: a
  , logNext :: v
  } deriving (Typeable, Functor)

instance SetMember Log (Log m) (Log m :> a)

class ShowLog l where
    showLog :: l -> String

instance ShowLog String where
    showLog s = s

-- | a monadic action that does the real logging
type Logger m l = forall v. Log l v -> m ()

logE :: (Typeable l, SetMember Log (Log l) r)
  => l -> Eff r ()
logE line = send $ \next -> inj (Log line (next ()))

runPureLog :: (Typeable l)
  => Eff (Log l :> r) a
  -> Eff r (a, [l])
runPureLog = go . admin
  where go (Val v) = return (v, [])
        go (E req) = handleRelay req go performLog
        performLog l = fmap (prefixLogWith l) (go (logNext l))
        prefixLogWith log' (v, l) = (v, logLine log' : l)

runLogStdErr :: (Typeable l, ShowLog l, SetMember Lift (Lift IO) r)
  => Eff (Log l :> r) a -> Eff r a
runLogStdErr = runLog stdErrLogger

stdErrLogger :: ShowLog l => Logger IO l
stdErrLogger = hPutStrLn stderr . showLog . logLine

runLog :: (Typeable l, Typeable1 m, SetMember Lift (Lift m) r)
  => Logger m l -> Eff (Log l :> r) a -> Eff r a
runLog logger = go . admin
  where go (Val v) = return v
        go (E req) = handleRelay req go performLog
        performLog l = lift (logger l) >> go (logNext l)

filterLog :: (Typeable l, SetMember Log (Log l) r)
  => (l -> Bool) -> Eff r a -> Eff r a
filterLog f = go . admin
  where go (Val v) = return v
        go (E req) = interpose req go filter'
          where filter' (Log l v) = if f l then send (<$> req) >>= go
                                           else go v
