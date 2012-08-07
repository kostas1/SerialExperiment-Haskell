module Serial ( 
    withDefaultSerial,
    withSerial,
    whenReadingSerial,
    writeSerial,
    writeDelayedSerial
    ) where
    
import System.Hardware.Serialport
import Data.Word
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString as B

port :: String
port = "COM4"

withDefaultSerial :: (SerialPort -> IO a) -> IO a
withDefaultSerial = withSerial port defaultSerialSettings { commSpeed = CS9600 }

whenReadingSerial :: SerialPort -> (B.ByteString -> IO ()) -> IO ()
whenReadingSerial s f =
    let get n =
            recv s 2 >>= \st ->
                if B.null st then
                    get $ B.append n st
                else unless (B.null n) $ f n
    in get B.empty

writeSerial :: SerialPort -> [Word8] -> IO Int
writeSerial s i = send s $ B.pack i

writeDelayedSerial :: SerialPort -> Int -> [[Word8]] -> IO [Int]
writeDelayedSerial s d l = forM l $ \i -> do
    r <- writeSerial s i
    threadDelay d
    return r