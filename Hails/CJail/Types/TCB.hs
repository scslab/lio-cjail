{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
-- | This module export a 'CJail' monad which is a wrapper for 'LIO' 
-- carrying a trusted configuration state. In fact 'CJail' is a Reader
-- monad that is not explicily an instance of 'MonadReader'. Jailed 
-- computations should be executed within this monad. The jail
-- configuration 'CJailConf' Show instance returns the string command
-- to @cjail@.
module Hails.CJail.Types.TCB ( -- * CJail monad
                               CJailConf(..)
                             , CJail(..)
                             , getCJailConf
                             , runCJail
                             , confToCmdArgs
                             ) where

import LIO
import Control.Monad.Reader


-- | A cjail configuration
data CJailConf = CJailConf { cjUser    :: Maybe String
                           -- ^ User
                           , cjTimeout :: Maybe Int
                           -- ^ Timeout in seconds
                           , cjDir     :: FilePath
                           -- ^ Path to jail
                           }

instance Show CJailConf where
  show conf = unwords
               [ "cjail"
               , maybe ""  ("--user "++) (cjUser conf)
               , maybe ""  (("--timeout "++) . show) (cjTimeout conf)
               , cjDir conf
               , "" ]

-- | CJailed monad is LIO + cjail state
newtype CJail l p s a = CJail { unCJail :: ReaderT CJailConf (LIO l p s) a }
  deriving (Functor, Monad)

instance LabelState l p s => MonadLIO (CJail l p s) l p s where
  liftLIO = CJail . liftLIO

-- | Get current configuration
getCJailConf :: CJail l p s CJailConf 
getCJailConf = CJail ask

-- | Execute a CJail computation
runCJail :: LabelState l p s => CJailConf -> CJail l p s a -> LIO l p s a
runCJail conf m = runReaderT (unCJail m) conf

-- | Convertconfiguration to pair of command and arguments.
confToCmdArgs :: CJailConf -> (String, [String])
confToCmdArgs conf = ("cjail", u ++ t ++ ["--", cjDir conf])
  where u = maybe [] (\x -> ["--user",x]) $ cjUser conf
        t = maybe [] (\x -> ["--timeout", show x ]) $ cjTimeout conf
