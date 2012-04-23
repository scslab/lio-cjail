{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif
{- | This module exports an interface that is similar to
"System.Process", but  uses labeled handles and labeled process
handles to communicate with a process.

The semantcs are different from "System.Process" in several ways:

* All the commands and executables are withing a @cjail@ and
  so peristance staorage lasts only for the duration of a 'runCJail'.

* Neither the environment nor file handles of the current process
  are in herited. New file handles are always created, and the 
  environment is by default empty.

-}
module Hails.CJail.System.Process.TCB ( -- * Running sub-processes
                                        createProcess
                                      , shell, proc
                                      , CreateProcess(..)
                                      , CmdSpec(..)
                                      , ProcessHandle
                                      , LProcessHandle(..)
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

import Prelude
import LIO
import LIO.MonadCatch
import LIO.Handle
import LIO.Handle.TCB (LHandle(..))
import LIO.TCB (rtioTCB, labelTCB)

import Hails.CJail.Types.TCB

import Data.Maybe

import Control.Monad

import System.Exit
import System.Process (CmdSpec(..), ProcessHandle)
import qualified System.Process as P

-- | Data structure specifying how a command should be created
data CreateProcess = CreateProcess 
  { cmdspec :: CmdSpec
  -- ^ Executable and arguments, or shell command
  , cwd     :: Maybe FilePath
  -- ^ Optional path to the working directory 
  , env     :: [(String, String)]
  -- ^ Environment for new process
  }

-- | Create a 'CreateProcess' representing a command to be passed to
-- the shell
shell :: String -> CreateProcess
shell cmd = CreateProcess { cmdspec = ShellCommand cmd
                          , cwd = Nothing
                          , env = [] }

-- | Create a 'CreateProcess' representing a raw command with arguements
proc :: FilePath -> [String] -> CreateProcess
proc cmd args = CreateProcess { cmdspec = RawCommand cmd args
                              , cwd = Nothing
                              , env = [] }

-- | Labeled handle to process
data LProcessHandle l = LProcessHandle
  { stdIn  :: LHandle l 
  -- ^ New standard in handle, will use default encoding and newline
  -- translation mode
  , stdOut :: LHandle l 
  -- ^ New standard out handle, will use default encoding and newline
  -- translation mode
  , stdErr :: LHandle l 
  -- ^ New standard error handle, will use default encoding and newline
  -- translation mode
  , processHandle :: Labeled l ProcessHandle
  -- ^ Handle to process
  }

instance Show CmdSpec where
  show (ShellCommand s) = s
  show (RawCommand fp args) = unwords $ fp : args

-- | Create a labeled process handle. The handle contains labeled
-- handles to standard in, standard out, and standard error. Moreover,
-- a labeled handle to the process itself (to e.g., terminate it) is
-- also constructed. Internally, this function calls
-- 'System.Process.createProcess'.
--
-- For example to execute a simple @ls@ command:
--
-- > import LIO.Handle
-- > import qualified Data.ByteString.Lazy.Char8 as L8
-- > ...
-- >
-- > ls :: LabelState l p s => LIO l p s L8.ByteString
-- > ls = runCJail (CJailConf Nothing Nothing "/opt/cjail/app0-jail") $ do
-- >        lph <- createProcess (shell "ls")
-- >        liftLIO $ hGetContents $ stdOut lph
--
-- or write and read from (same) temporary file with @cat@:
--
-- > ex :: LabelState l p s => LIO l p s L8.ByteString
-- > ex = runCJail (CJailConf Nothing Nothing "/opt/cjail/app0-jail") $ do
-- >       lph <- createProcess (shell "cat > /tmp/xxx ; cat /tmp/xxx")
-- >       liftLIO $ hPutStrLn (stdIn lph) (L8.pack "hello jail")
-- >       liftLIO $ hClose (stdIn lph)
-- >       liftLIO $ closeHandles lph
--
-- Note that both of these examples use Lazy IO and thus the handles
-- are not closed. More appropriately, the result from the jailed
-- process should be forced (with e.g., 'evaluate') and the handles
-- should be closed. Consider for example sorting a list of numbers
-- with @sort@:
--
-- > sort :: LabelState l p s => [Int] -> LIO l p s [Int]
-- > sort ls = do
-- >   lph <- runCJail (CJailConf Nothing Nothing "/opt/cjail/app0-jail") $
-- >            createProcess (proc "sort" ["-n"])
-- >   let input = L8.pack . intercalate "\n" . map show $ ls
-- >   hPut (stdIn lph) input
-- >   hClose (stdIn lph)
-- >   bs <- whileNotEOF (stdOut lph) []
-- >   closeHandles lph
-- >   return bs
-- >     where whileNotEOF h acc = do
-- >             eof <- hIsEOF  h
-- >             if eof
-- >               then return acc
-- >               else do res <- (read . L8.unpack) `liftM` hGetLine h
-- >                       whileNotEOF h (res : acc)
--
--
createProcess :: LabelState l p s
              => CreateProcess -> CJail l p s (LProcessHandle l)
createProcess cp = do
  liftLIO . guardCmd . show . cmdspec $ cp
  conf <- getCJailConf
  liftLIO $ do
    l <- getLabel
    (mh0, mh1, mh2, ph) <- rtioTCB $ P.createProcess $ mkCreateProcess conf cp
    [h0, h1, h2] <- getHandlesOrError [mh0, mh1, mh2] ph
    return LProcessHandle { stdIn  = LHandleTCB l h0
                          , stdOut = LHandleTCB l h1
                          , stdErr = LHandleTCB l h2
                          , processHandle = labelTCB l ph }
    where getHandlesOrError mhs ph =
            let hs = catMaybes mhs
            in if length mhs == length hs
                 then return hs
                 else do rtioTCB $ do mapM_ hClose hs
                                      void $ P.terminateProcess ph
                         throwIO $ userError 
                            "createProcess could not create standard handles"

-- | Run command using the shell. Handles are initially in binary mode.
runInteractiveCommand :: LabelState l p s
                      => String -> CJail l p s (LProcessHandle l)
runInteractiveCommand cmd = do
  liftLIO $ guardCmd cmd
  conf <- getCJailConf
  liftLIO $ do
    l <- getLabel
    (h0, h1, h2, ph) <- rtioTCB $ P.runInteractiveCommand $ show conf ++ cmd
    return LProcessHandle { stdIn  = LHandleTCB l h0
                          , stdOut = LHandleTCB l h1
                          , stdErr = LHandleTCB l h2
                          , processHandle = labelTCB l ph }

-- | Run a raw command. Handles are initially in binary mode.
runInteractiveProcess :: LabelState l p s
                      => FilePath           -- ^ Executable
                      -> [String]           -- ^ Arguments
                      -> Maybe FilePath     -- ^ Optional current working dir
                      -> [(String, String)] -- ^ Environment
                      -> CJail l p s (LProcessHandle l)
runInteractiveProcess exe args mpath e = do
  liftLIO $ do guardMany (exe : args)
               maybe (return ()) guardCmd $ mpath
  conf <- getCJailConf
  liftLIO $ do
    l <- getLabel
    (h0, h1, h2, ph) <- rtioTCB $ P.runInteractiveProcess (show conf ++ exe)
                                                          args
                                                          mpath
                                                          (Just e)
    return LProcessHandle { stdIn  = LHandleTCB l h0
                          , stdOut = LHandleTCB l h1
                          , stdErr = LHandleTCB l h2
                          , processHandle = labelTCB l ph }

-- | Fork an external process and read it standard output strictly,
-- blocking until the process terminates and retuns an output string.
-- The function throws an 'IOError' if the exit code is not 'ExitSuccess'
-- Must compile with @-threaded@ if you want other threads to keep running
-- while blocking on the result of @readProcess@
readProcess :: LabelState l p s
            => FilePath            -- ^ Executable
            -> [String]            -- ^ Arguments
            -> String              -- ^ Standard input
            -> CJail l p s String  -- ^ Standard output
readProcess exe args stdin = do
  liftLIO $ guardMany (exe : args)
  conf <- getCJailConf
  liftLIO $ rtioTCB $ P.readProcess (show conf ++ exe) args stdin

-- | Same as 'readProcess', but returns the exit code explicitly, and
-- strictly reads standard error.
readProcessWithExitCode :: LabelState l p s
  => FilePath            -- ^ Executable
  -> [String]            -- ^ Arguments
  -> String              -- ^ Standard input
  -> CJail l p s (ExitCode, String, String)  -- ^ (exit code, stdout, stderr)
readProcessWithExitCode exe args stdin = do
  liftLIO $ guardMany (exe : args)
  conf <- getCJailConf
  liftLIO $ rtioTCB $ P.readProcessWithExitCode (show conf ++ exe) args stdin

-- | Computation @system cmd@ returns the exit code produced when
-- running shell command. Note that standard output and standard error
-- are redirected to @/dev/null/@.
system :: LabelState l p s => String -> CJail l p s ExitCode
system cmd = do
  liftLIO $ guardCmd cmd
  conf <- getCJailConf
  liftLIO $ rtioTCB $ P.system $
    show conf ++ cmd ++ " >& /dev/null"

--
-- Process completion
--

-- | Wait for specified process to terminate. This function raises the
-- current label to the label of the process handle.
-- Must compile with @-threaded@ if you want other threads to keep running
-- while blocking on the result.
waitForProcess :: LabelState l p s 
                => LProcessHandle l -> LIO l p s ExitCode
waitForProcess = waitForProcessP noPrivs

-- | Same as 'waitForProcess', but uses privileges when raising current
-- label.
waitForProcessP :: LabelState l p s 
                => p -> LProcessHandle l -> LIO l p s ExitCode
waitForProcessP p' lph = withCombinedPrivs p' $ \p -> do
  ph <- unlabelP p $ processHandle lph
  rtioTCB $ P.waitForProcess ph

-- | Get process exit code without blocking. This function raises the
-- current label to the label of the process handle.
getProcessExitCode :: LabelState l p s 
                   => LProcessHandle l -> LIO l p s (Maybe ExitCode)
getProcessExitCode = getProcessExitCodeP noPrivs

-- | Same as 'getProcessExitCode', but uses privileges when raising current
-- label.
getProcessExitCodeP :: LabelState l p s 
                    => p -> LProcessHandle l -> LIO l p s (Maybe ExitCode)
getProcessExitCodeP p' lph = withCombinedPrivs p' $ \p -> do
  ph <- unlabelP p $ processHandle lph
  rtioTCB $ P.getProcessExitCode ph

-- | Attempt to terminate the specified process.
-- As noted in "System.Process", this function should not be used under
-- normal circumstances. This function sends the process the @SIGTERM@
-- signal.
-- It must be that the the current computation can both read and write
-- the process handle. Furthermore, the
-- current label is raised to the label of the process handle.
terminateProcess :: LabelState l p s 
                   => LProcessHandle l -> LIO l p s ()
terminateProcess = terminateProcessP noPrivs

-- | Same as 'terminateProcess', but uses privileges when raising current
-- label.
terminateProcessP :: LabelState l p s 
                    => p -> LProcessHandle l -> LIO l p s ()
terminateProcessP p' lph = withCombinedPrivs p' $ \p -> do
  wguardP p $ labelOf (processHandle lph)
  ph <- unlabelP p $ processHandle lph
  rtioTCB $ P.terminateProcess ph
  

-- | Close all handles (simply calls 'hClose')
closeHandles :: LabelState l p s => LProcessHandle l -> LIO l p s ()
closeHandles = closeHandlesP noPrivs

-- | Close all handles, but uses privileges
closeHandlesP :: LabelState l p s => p -> LProcessHandle l -> LIO l p s ()
closeHandlesP p' lph = withCombinedPrivs p' $ \p -> 
  mapM_ (safeHClose p) [stdIn lph, stdOut lph, stdErr lph]
  where safeHClose p h = hCloseP p h `onException` return ()

--
-- Helpers
--

-- | Make a @CreateProcess@ value usable by "System.Process"
mkCreateProcess :: CJailConf -> CreateProcess -> P.CreateProcess
mkCreateProcess conf cp = P.CreateProcess 
  { P.cmdspec      = ShellCommand $ show conf ++ show (cmdspec cp)
  , P.cwd          = cwd cp
  , P.env          = Just $ env cp
  , P.std_in       = P.CreatePipe
  , P.std_out      = P.CreatePipe
  , P.std_err      = P.CreatePipe
  , P.close_fds    = True
  , P.create_group = False }


