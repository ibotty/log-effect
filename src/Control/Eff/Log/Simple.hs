{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
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

import Control.Eff           (Eff, Member)
import Control.Eff.Log
import Data.Monoid           ((<>))
import Data.Typeable         (Typeable)
import Prelude               hiding (error)
import System.Log.FastLogger (ToLogStr, toLogStr)

data Severity =
    DEBUG | INFO | NOTICE | WARNING | ERROR | CRITICAL | ALERT | PANIC
  deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable)

type SimpleLog a = Log (Severity, a)

instance ToLogStr Severity where
    toLogStr DEBUG    = "DEBUG"
    toLogStr INFO     = "INFO"
    toLogStr NOTICE   = "NOTICE"
    toLogStr WARNING  = "WARNING"
    toLogStr ERROR    = "ERROR"
    toLogStr CRITICAL = "CRITICAL"
    toLogStr ALERT    = "ALERT"
    toLogStr PANIC    = "PANIC"

instance ToLogStr a => ToLogStr (Severity, a) where
    toLogStr (sev, line) = "[" <> toLogStr sev <> "] " <> toLogStr line <> "\n"

logTo :: (Typeable l, Member (Log (Severity, l)) r)
  => Severity -> l -> Eff r ()
logTo sev line = logE (sev, line)

debug :: (Typeable l, Member (Log (Severity, l)) r) => l -> Eff r ()
debug = logTo DEBUG

info :: (Typeable l, Member (Log (Severity, l)) r) => l -> Eff r ()
info = logTo INFO

notice :: (Typeable l, Member (Log (Severity, l)) r) => l -> Eff r ()
notice = logTo NOTICE

warning :: (Typeable l, Member (Log (Severity, l)) r) => l -> Eff r ()
warning = logTo WARNING

error :: (Typeable l, Member (Log (Severity, l)) r) => l -> Eff r ()
error = logTo ERROR

critical :: (Typeable l, Member (Log (Severity, l)) r) => l -> Eff r ()
critical = logTo CRITICAL

alert :: (Typeable l, Member (Log (Severity, l)) r) => l -> Eff r ()
alert = logTo ALERT

panic :: (Typeable l, Member (Log (Severity, l)) r) => l -> Eff r ()
panic = logTo PANIC
