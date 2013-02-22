{- |
  This module export the 'CJail' monad in which untrusted code can
  spawn new processes and communicate with them. 'CJail' is a wrapper
  for 'LIO', encapsulating the jail configuration that untrusted code
  should not be able to modify.
-}

{-# LANGUAGE Unsafe
           , MultiParamTypeClasses
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , DeriveFunctor #-}


module LIO.CJail.System.Process.TCB ( -- * CJail Monad
                                      CJail(..), evalCJail
                                    , CJailConf(..)
                                    , getCJailConfTCB 
                                    ) where

import           LIO
import           LIO.TCB (ShowTCB(..))
import           Control.Monad.Reader
import           Control.Applicative
import qualified CJail.System.Process as C

-- | Data-type used to encode the jail configuration information.
newtype CJailConf = CJailConfTCB  { unCJailConfTCB :: C.CJailConf }

instance ShowTCB CJailConf where
  showTCB = show . unCJailConfTCB 

-- | CJailed monad wrapper for the "LIO" monad.
newtype CJail l a = CJailTCB { unCJailTCB :: ReaderT CJailConf (LIO l) a }
  deriving (Functor, Applicative, Monad)

instance Label l => MonadLIO l (CJail l) where
  liftLIO = CJailTCB . lift

-- | Execute a CJail computation
evalCJail :: Label l => CJailConf -> CJail l a -> LIO l a
evalCJail conf m = runReaderT (unCJailTCB m) conf

-- | Get underlying configuration
getCJailConfTCB :: CJail l CJailConf
getCJailConfTCB = CJailTCB ask
