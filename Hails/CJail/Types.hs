{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Safe interface to "Hails.CJail.Types.TCB".
module Hails.CJail.Types ( -- * CJail monad
                           CJailConf, cjUser, cjTimeout, cjDir
                         , CJail, runCJail
                         , getCJailConf
                         ) where
import Hails.CJail.Types.TCB
