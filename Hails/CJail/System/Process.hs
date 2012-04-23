{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Safe interface to "Hails.CJail.System.Process.TCB".
module Hails.CJail.System.Process ( -- * Running sub-processes
                                    createProcess
                                  , shell, proc
                                  , CreateProcess(..)
                                  , CmdSpec(..)
                                  , ProcessHandle
                                  , LProcessHandle
                                  , stdIn, stdOut, stdErr, processHandle
                                  -- ** Specific variants of createProcess
                                  , runInteractiveCommand
                                  , runInteractiveProcess
                                  , readProcess
                                  , readProcessWithExitCode
                                  , system
                                  -- * Process completion
                                  , waitForProcess, waitForProcessP
                                  , getProcessExitCode, getProcessExitCodeP
                                  , terminateProcess, terminateProcessP
                                  , closeHandles, closeHandlesP
                                  ) where

import Hails.CJail.System.Process.TCB
