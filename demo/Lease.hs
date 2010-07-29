-- Paxos - A Haskell library implementing several Paxos-related algorithms
--
-- Copyright (C) 2010  Nicolas Trangez
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation;
-- version 2.1 of the License.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
-- 02110-1301  USA

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.List (intercalate)

import qualified System.Log.Logger as HSL

import Network.Paxos.Lease

versionString :: String
versionString = intercalate "." $ map show $ tupleToList3 version
    where tupleToList3 :: (a, a, a) -> [a]
          tupleToList3 (a, b, c) = [a, b, c]

main :: IO ()
main = do
    HSL.updateGlobalLogger HSL.rootLoggerName (HSL.setLevel HSL.DEBUG)

    putStrLn $ "PaxosLease v" ++ versionString

    s1 <- createState 1 5
    s2 <- createState 2 5
    s3 <- createState 3 5
    s4 <- createState 4 5
    s5 <- createState 5 5

    (Broadcast r1, s1') <- runPaxosLease propose s1

    (Reply r2, s2') <- runPaxosLease (handleMessage r1) s2
    (Ignore, s1'') <- runPaxosLease (handleMessage r2) s1'

    (Reply r3, s3') <- runPaxosLease (handleMessage r1) s3
    (Ignore, s1''') <- runPaxosLease (handleMessage r3) s1''

    (Reply r4, _) <- runPaxosLease (handleMessage r1) s4
    (Broadcast r5, s1'''') <- runPaxosLease (handleMessage r4) s1'''

    (Reply r6, _) <- runPaxosLease (handleMessage r1) s5
    (Broadcast r7, s1''''') <- runPaxosLease (handleMessage r6) s1''''

    (Reply r8, s2'') <- runPaxosLease (handleMessage r5) s2'
    (Reply r9, _) <- runPaxosLease (handleMessage r7) s2''
    (Ignore, s1'''''') <- runPaxosLease (handleMessage r8) s1'''''
    (Ignore, s1''''''') <- runPaxosLease (handleMessage r9) s1''''''

    (Reply r10, s3'') <- runPaxosLease (handleMessage r5) s3'
    (Reply r11, _) <- runPaxosLease (handleMessage r7) s3''
    (Ignore, s1'''''''') <- runPaxosLease (handleMessage r10) s1'''''''
    (Ignore, _) <- runPaxosLease ( handleMessage r11) s1''''''''

    return ()
