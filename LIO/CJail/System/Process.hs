{-# LANGUAGE Trustworthy #-}
{-
  This module export the 'CJail' monad in which untrusted code can
  spawn new processes and communicate with them. 'CJail' is a wrapper
  for 'LIO', encapsulating the jail configuration that untrusted code
  should not be able to modify.
-}
module LIO.CJail.System.Process (
    -- * CJail Monad
    CJail, evalCJail
  , CJailConf
  -- * Running sub-processes
  , createProcessL
  , createProcess
  , createProcessP
  , shell, proc
  , CreateProcess(..)
  , CmdSpec(..)
  , LabeledProcessHandle(..)
  -- ** Specific variants of createProcess
  , readProcess
  , readProcessWithExitCode
  -- * Process completion
  , waitForProcess, waitForProcessP
  , getProcessExitCode, getProcessExitCodeP
  , terminateProcess, terminateProcessP
  , closeHandles, closeHandlesP
  ) where

import           Data.Binary
import           Control.Monad

import           LIO
import           LIO.Handle
import           LIO.TCB (rethrowIoTCB)
import           LIO.CJail.System.Process.TCB

import           CJail.System.Process ( shell, proc
                                      , CreateProcess(..)
                                      , CmdSpec(..) )
import qualified CJail.System.Process as C
import qualified System.Process as P
import           System.Exit

--
-- Handles
--

-- | Labeled handle to process
data LabeledProcessHandle l = LabeledProcessHandle
  { stdIn  :: LabeledHandle l
  -- ^ New standard in handle, will use default encoding and newline
  -- translation mode
  , stdOut :: LabeledHandle l
  -- ^ New standard out handle, will use default encoding and newline
  -- translation mode
  , stdErr :: LabeledHandle l
  -- ^ New standard error handle, will use default encoding and newline
  -- translation mode
  , processHandle :: Labeled l P.ProcessHandle
  -- ^ Handle to process
  }

--
--
--

-- | Create a process handle. The handle contains labeled handles to
-- standard in, standard out, and standard error. Moreover, a handle
-- to the process itself  is also constructed. Internally, this
-- function calls 'System.Process.createProcess'. The label of the
-- process handle must be bounded by the current label and clerance.
--
-- For example to execute a simple @ls@ command:
--
-- > import qualified Data.ByteString.Lazy.Char8 as L8
-- > ...
-- >
-- > ls :: (Label l, Binary l) => LIO l L8.ByteString
-- > ls = evalCJail (cJailConfTCB Nothing Nothing jail_path) $ do
-- >   lph <- createProcessL (shell "ls")
-- >   liftLIO $ hGetContents $ stdOut lph
--
-- or write and read from (same) temporary file with @cat@:
--
-- > ex :: (Label l, Binary l) => LIO l L8.ByteString
-- > ex = evalCJail (cJailConfTCB Nothing Nothing jail_path) $ do
-- >   lph <- createProcessL (shell "cat > /tmp/xxx ; cat /tmp/xxx")
-- >   hPutStrLn (stdIn lph) (L8.pack "hello jail")
-- >   hClose (stdIn lph)
-- >   hGetContents $ stdOut lph
--
-- Note that both of these examples use Lazy IO and thus the handles
-- are not closed. More appropriately, the result from the jailed
-- process should be forced (with e.g., 'evaluate') and the handles
-- should be closed. Consider for example sorting a list of numbers
-- with @sort@:
--
-- > sort :: (Label l, Binary l) => [Int] -> LIO l [Int]
-- > sort xs = evalCJail (cJailConfTCB Nothing Nothing jail_path) $ do
-- >   lph <- createProcessL (proc "sort" ["-n"])
-- >   let input = L8.pack . intercalate "\n" . map show $ xs
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
createProcess :: Label l
              => l -> CreateProcess -> CJail l (LabeledProcessHandle l)
createProcess = createProcessP NoPrivs


-- | Same as 'createProcess', but uses privileges.
createProcessP :: Priv l p 
               => p -> l -> CreateProcess -> CJail l (LabeledProcessHandle l)
createProcessP p l cp = do
  guardAllocP p l
  conf <- unCJailConfTCB `liftM` getCJailConfTCB
  liftLIO $ do
    ph <- rethrowIoTCB $ C.createProcess conf cp
    li <- labelP p l $ C.stdIn  ph 
    lo <- labelP p l $ C.stdOut ph 
    le <- labelP p l $ C.stdErr ph 
    lp <- labelP p l $ C.processHandle ph
    return $ LabeledProcessHandle { stdIn         = li
                                  , stdOut        = lo
                                  , stdErr        = le
                                  , processHandle = lp }

-- | Same as 'createProcess', but uses the current label as the label
-- of the handles.
createProcessL :: Label l => CreateProcess -> CJail l (LabeledProcessHandle l)
createProcessL cp = do
  l <- getLabel 
  createProcess l cp

-- | Fork an external process and read it standard output strictly,
-- blocking until the process terminates and retuns an output string.
-- The function throws an 'IOError' if the exit code is not 'ExitSuccess'
-- Must compile with @-threaded@ if you want other threads to keep running
-- while blocking on the result of @readProcess@
readProcess :: Label l 
            => FilePath        -- ^ Executable
            -> [String]        -- ^ Arguments
            -> String          -- ^ Standard input
            -> CJail l String  -- ^ Standard output
readProcess exe exeArgs stdin' = do
  conf <- unCJailConfTCB `liftM` getCJailConfTCB
  liftLIO . rethrowIoTCB $ C.readProcess conf exe exeArgs stdin'

-- | Same as 'readProcess', but returns the exit code explicitly, and
-- strictly reads standard error.
readProcessWithExitCode ::  Label l
                        => FilePath -- ^ Executable
                        -> [String] -- ^ Arguments
                        -> String   -- ^ Standard input
                        -> CJail l (ExitCode, String, String)
                           -- ^ (exit code, stdout, stderr)
readProcessWithExitCode exe exeArgs stdin' = do
  conf <- unCJailConfTCB `liftM` getCJailConfTCB
  liftLIO . rethrowIoTCB $ C.readProcessWithExitCode conf exe exeArgs stdin'

--
-- Process completion
--

-- | Wait for specified process to terminate.  Must compile with
-- @-threaded@ if you want other threads to keep running while blocking
-- on the result.
waitForProcess :: Label l => LabeledProcessHandle l -> CJail l ExitCode
waitForProcess = waitForProcessP NoPrivs

-- | Same as 'waitForProcess', but uses privileges.
waitForProcessP :: Priv l p => p -> LabeledProcessHandle l -> CJail l ExitCode
waitForProcessP p lph = do
  ph <- unlabelP p $ processHandle lph
  liftLIO . rethrowIoTCB $ P.waitForProcess ph

-- | Get process exit code without blocking.
getProcessExitCode :: Label l 
                   => LabeledProcessHandle l -> CJail l (Maybe ExitCode)
getProcessExitCode = getProcessExitCodeP NoPrivs

-- | Same as 'getProcessExitCode', but uses privileges.
getProcessExitCodeP :: Priv l p 
                    => p ->  LabeledProcessHandle l -> CJail l (Maybe ExitCode)
getProcessExitCodeP p lph = do
  ph <- unlabelP p $ processHandle lph
  liftLIO . rethrowIoTCB $ P.getProcessExitCode ph

-- | Attempt to terminate the specified process.
-- As noted in "System.Process", this function should not be used under
-- normal circumstances. This function sends the process the @SIGTERM@
-- signal.
terminateProcess :: Label l => LabeledProcessHandle l -> CJail l ()
terminateProcess = terminateProcessP NoPrivs

-- | Same as 'terminateProcess', but uses privileges.
terminateProcessP :: Priv l p => p -> LabeledProcessHandle l -> CJail l ()
terminateProcessP p lph = do
  guardWriteP p $ labelOf $ processHandle lph
  ph <- unlabelP p $ processHandle lph
  liftLIO . rethrowIoTCB $ P.terminateProcess ph
  

-- | Close all handles (simply calls 'hClose')
closeHandles :: (Label l, Binary l) 
             => LabeledProcessHandle l -> CJail l ()
closeHandles = closeHandlesP NoPrivs

-- | Same as 'closeHandles', but uses privileges.
closeHandlesP :: (Priv l p, Binary l) 
              => p -> LabeledProcessHandle l -> CJail l ()
closeHandlesP p lph = liftLIO $ mapM_ safeHClose [stdIn lph
                                                 , stdOut lph
                                                 , stdErr lph]
  where safeHClose h = hCloseP p h `onException` return ()
