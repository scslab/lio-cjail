{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-|
This module rexports an interface for executing arbitrary code in a
jailed environment provided by @cjail@ in the 'CJail' monad.  The
interface for executing arbitrary jailed processes is like that of
"System.Process".  See "LIO.CJail.System.Process.hs".

Utnrusted code cannot construct arbitrary CJail environment
configurations (i.e., values of type CJailConf). Instead, trusted code
must create such a value or 'inCJail' must be used. In the latter case
the environment is read for variables:

* @LIO_CJAIL_TIMEOUT@ corresponds to the @--timeout@ option.

* @LIO_CJAIL_DIR@ corresponds to the @cjail@ jail.

The @LIO_CJAIL_DIR@ variable must be defined, otherwise 'inJail'
throws an exception.

-}
module LIO.CJail ( inCJail
                 , module LIO.CJail.System.Process
                 ) where

import Data.Maybe (listToMaybe)

import LIO
import LIO.TCB (rethrowIoTCB)
import LIO.CJail.System.Process
import LIO.CJail.System.Process.TCB (CJailConf(..))
import qualified CJail.System.Process as C

import System.Environment

-- | Execute a jailed process in a cjail, configured according to the
-- current environment.
inCJail :: Label l => CJail l a -> LIO l a
inCJail io = do
  e <- rethrowIoTCB getEnvironment
  case lookup "LIO_CJAIL_DIR" e of
    Nothing  -> throwLIO . userError $ "CJail not configured"
    Just dir -> evalCJail (CJailConfTCB $ 
                            C.CJailConf { C.cjUser = Nothing
                                        , C.cjTimeout = toutFromEnv
                                        , C.cjDir = dir }) io
        where toutFromEnv = do tS <- lookup "LIO_CJAIL_TIMEOUT" e
                               maybeRead tS
              maybeRead :: Read a => String -> Maybe a
              maybeRead = fmap fst . listToMaybe . reads
