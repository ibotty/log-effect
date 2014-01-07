{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Control.Eff.Log.Simple
  ( SimpleLog
  , Severity(..)
  , logTo
  , debug
  , info
  , notice
  , warning
  , error
  , critical
  , alert
  , panic
  ) where

import Prelude hiding (error)
import Control.Eff (Eff, SetMember)
import Control.Eff.Log
import Data.Typeable (Typeable)

data Severity = DEBUG | INFO | NOTICE | WARNING | ERROR | CRITICAL | ALERT | PANIC
  deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable)

logTo :: (Typeable l, SetMember Log (Log (Severity, l)) r)
  => Severity -> l -> Eff r ()
logTo sev line = logE (sev, line)

debug :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
debug = logTo DEBUG

info :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
info = logTo INFO

notice :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
notice = logTo NOTICE

warning :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
warning = logTo WARNING

error :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
error = logTo ERROR

critical :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
critical = logTo CRITICAL

alert :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
alert = logTo ALERT

panic :: (Typeable l, SetMember Log (Log (Severity, l)) r) => l -> Eff r ()
panic = logTo PANIC

type SimpleLog = Log (Severity, String)

instance ShowLog a => ShowLog (Severity, a) where
    showLog (sev, line) = "[" ++ show sev ++ "] " ++ showLog line
