module Main (
	main
	) where

import System.Environment
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as C8
import qualified Database.Redis as R
import qualified Data.Text as T
import Database.Redis.Dump.JSON

main = do
	[pat] <- getArgs
	c <- R.connect R.defaultConnectInfo
	r <- R.runRedis c $ dump (T.pack pat)
	L.putStrLn r
