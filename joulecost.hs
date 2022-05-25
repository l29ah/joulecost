import Control.Concurrent
import Control.Loop
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Int
import Data.Maybe
import System.Environment
import System.IO
import System.Posix.Clock
import System.Process

callProcessSilent :: Handle -> FilePath -> [String] -> IO ()
callProcessSilent out cmd args = do
	withCreateProcess
		(proc cmd args) { std_out = UseHandle out, delegate_ctlc = True } $ \_ _ _ p ->
		waitForProcess p
	pure ()

getMicrojoules :: IO Int64
getMicrojoules = fromIntegral . fst . fromJust . BS.readInteger <$> BS.readFile "/sys/class/powercap/intel-rapl:0/energy_uj"

-- |Returns seconds and joules spent
measure :: IO a -> IO (a, (Double, Double))
measure action = do
	start <- getClockTime monotonicClock
	before <- getMicrojoules
	result <- action
	end <- getClockTime monotonicClock
	after <- getMicrojoules
	let nanoseconds = fromIntegral $ timeSpecToInt64 $ end - start
	let microjoules = fromIntegral $ after - before
	pure (result, (nanoseconds / 1e9, microjoules / 1e6))

main = do
	putStrLn "Calibrating..."
	(_, (idleTime, idleEnergy)) <- measure $ threadDelay 5000000
	let idlePower = idleEnergy / idleTime
	putStrLn $ "Idle power: " ++ (show idlePower) ++ "W"
	putStrLn "Running the target application..."
	program : times : [] <- getArgs
	(time, energy) <- numLoopState 0 (read times) (0, 0) $ \(timeAcc, energyAcc) _ -> do
		null <- openFile "/dev/null" WriteMode
		(_, (newTime, newEnergy)) <- measure $ callProcessSilent null program []
		pure (timeAcc + newTime, energyAcc + newEnergy)
	when (time < 5) $ putStrLn "Warning: the process exited too quickly for an accurate measurement"
	let power = energy / time
	putStrLn $ "Process power: " ++ (show power) ++ "W"
