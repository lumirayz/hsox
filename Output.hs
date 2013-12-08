module Output
	( outputSox
	) where

import Types

import System.Process
import System.IO

import qualified Data.ByteString.Lazy as BL

import Blaze.ByteString.Builder

import Data.Monoid
import Data.Word

import Control.Monad

import Control.Concurrent

uint16_max = 2 ** 16

mergePCM :: [Word16] -> BL.ByteString
mergePCM = toLazyByteString . fromWrite . mconcat . map writeWord16le

renderPCM :: Osc -> Time -> Word16
renderPCM osc = fromIntegral . floor . (* uint16_max) . (/ 2) . (+ 1) . sample osc

runSox :: Int -> IO (Handle, ProcessHandle)
runSox sampleRate = do
	(sin, sout, serr, p) <- runInteractiveProcess "play"
		[ "-t", "raw"
		, "-r", show sampleRate
		, "-b", "16"
		, "-c", "1"
		, "-e", "unsigned-integer"
		, "-L"
		, "-"
		] Nothing Nothing
	forkIO $ forever $ hGetLine serr
	return (sin, p)

outputSox :: Int -> Int -> Osc -> IO ()
outputSox sampleRate seconds osc = do
	(sin, p) <- runSox sampleRate
	forM_ [0..seconds * sampleRate] $ \t -> do
		let time = fromIntegral t / fromIntegral sampleRate
		BL.hPut sin $ mergePCM [renderPCM osc time]
	void $ waitForProcess p
