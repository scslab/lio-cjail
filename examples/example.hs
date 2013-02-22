import Control.Monad
import Data.Binary
import LIO
import LIO.DCLabel
import LIO.Handle
import LIO.TCB (ioTCB)
import LIO.CJail
import LIO.CJail.System.Process.TCB
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (intercalate)

jail_path :: FilePath
jail_path = "/opt/cjail/tmp-jail"

main :: IO ()
main = evalDC $ do
  ls >>= ioTCB . L8.putStrLn
  sort [2,38,1230,1234,1123,45,980,77,87] >>= ioTCB . putStrLn . show
  ex >>= ioTCB . L8.putStrLn

ls :: (Label l, Binary l) => LIO l L8.ByteString
ls = evalCJail (cJailConfTCB Nothing Nothing jail_path) $ do
  lph <- createProcessL (shell "ls")
  liftLIO $ hGetContents $ stdOut lph

sort :: (Label l, Binary l) => [Int] -> LIO l [Int]
sort xs = evalCJail (cJailConfTCB Nothing Nothing jail_path) $ do
  lph <- createProcessL (proc "sort" ["-n"])
  let input = L8.pack . intercalate "\n" . map show $ xs
  hPut (stdIn lph) input
  hClose (stdIn lph)
  bs <- whileNotEOF (stdOut lph) []
  closeHandles lph
  return bs
    where whileNotEOF h acc = do
            eof <- hIsEOF  h
            if eof
              then return acc
              else do res <- (read . L8.unpack) `liftM` hGetLine h
                      whileNotEOF h (res : acc)

ex :: (Label l, Binary l) => LIO l L8.ByteString
ex = evalCJail (cJailConfTCB Nothing Nothing jail_path) $ do
  lph <- createProcessL (shell "cat > /tmp/xxx ; cat /tmp/xxx")
  hPutStrLn (stdIn lph) (L8.pack "hello jail")
  hClose (stdIn lph)
  hGetContents $ stdOut lph
