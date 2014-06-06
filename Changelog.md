# Changelog

## Version 0.4

 * add a proxy to most `runLog` functions to not have to supply the full
   specialized type signature. Use something like `data Proxy a = Proxy` or
   the one from `tagged`.

 * remove `ShowLog` typeclass in favor of `ToLogString` from `fast-logger`.
