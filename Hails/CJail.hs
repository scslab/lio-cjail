{-|
This module rexports an interface for executing arbitrary code in a
jailed environment provided by @cjail@ in the 'CJail' monad.  The
interface for executing arbitrary jailed processes is like that of
"System.Process". See the exported modules for fruther documentation.
-}
module Hails.CJail ( module Hails.CJail.Types
                   , module Hails.CJail.System.Process
                   ) where

import Hails.CJail.Types
import Hails.CJail.System.Process
