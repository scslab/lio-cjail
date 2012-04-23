{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-|
This module rexports an interface for executing arbitrary code in a
jailed environment provided by @cjail@ in the 'CJail' monad.  The
interface for executing arbitrary jailed processes is like that of
"System.Process". See the exported modules for further documentation.

Utnrusted code cannot construct arbitrary CJail environment
configurations (i.e., values of type CJailConf). Instead, trusted code
must create such a value or 'inCJail' must be used. In the latter case
the environment is read for variables:

* @HAILS_CJAIL_TIMEOUT@ corresponds to the @--timeout@ option.

* @HAILS_CJAIL_DIR@ corresponds to the @cjail@ jail.

The @HAILS_CJAIL_DIR@ variable must be defined, otherwise 'inJail'
throws an exception.

-}
module Hails.CJail ( inCJail
                   , module Hails.CJail.Types
                   , module Hails.CJail.System.Process
                   ) where

import Hails.CJail.Types
import Hails.CJail.System.Process

import Hails.CJail.Types.TCB (CJailConf(..))

import Data.Maybe (listToMaybe)

import LIO.TCB
import System.Environment

-- | Execute a jailed process in a cjail defined according to the
-- environment.
inCJail :: LabelState l p s => CJail l p s a -> LIO l p s a
inCJail io = do
  e <- rtioTCB getEnvironment
  case lookup "HAILS_CJAIL_DIR" e of
    Nothing -> throwIO . userError $ "Jailed processes not supported"
    Just dir -> runCJail CJailConf { cjUser = Nothing
                                   , cjTimeout = toutFromEnv
                                   , cjDir = dir } io
        where toutFromEnv = do tS <- lookup "HAILS_CJAIL_TIMEOUT" e
                               maybeRead tS
              maybeRead :: Read a => String -> Maybe a
              maybeRead = fmap fst . listToMaybe . reads
