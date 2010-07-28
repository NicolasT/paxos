{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.List (intercalate)

import System.Log.Logger.TH (deriveLoggers)
import qualified System.Log.Logger as HSL

import Network.Paxos.Lease (version)

$(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO])

versionString :: String
versionString = intercalate "." $ map show $ tupleToList3 version
    where tupleToList3 :: (a, a, a) -> [a]
          tupleToList3 (a, b, c) = [a, b, c]

main :: IO ()
main = do
    putStrLn $ "PaxosLease v" ++ versionString
    forkIO runServer