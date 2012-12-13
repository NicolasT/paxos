{- Paxos - Implementations of Paxos-related consensus algorithms
 -
 - Copyright (C) 2012  Nicolas Trangez
 -
 - This library is free software; you can redistribute it and/or
 - modify it under the terms of the GNU Lesser General Public
 - License as published by the Free Software Foundation; either
 - version 2.1 of the License, or (at your option) any later version.
 -
 - This library is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 - Lesser General Public License for more details.
 -
 - You should have received a copy of the GNU Lesser General Public
 - License along with this library; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
 - USA.
 -}

module Control.Concurrent.UnreliableChan (
      Chan
    , readChan
    , writeChan
    ) where

import Control.Monad

import Control.Concurrent hiding (Chan, readChan, writeChan)
import Control.Concurrent.STM hiding (readTChan)
import qualified Control.Concurrent.STM as TChan

import System.Random

type Chan a = TChan a

-- | Write a value to a `Chan', rather unreliably
writeChan :: Double  -- ^ Message loss probability
          -> (Int, Int)  -- ^ Message delivery delay bounds (in microseconds)
          -> Chan a  -- ^ `Chan' to write to
          -> a  -- ^ Value to deliver
          -> IO ()
writeChan lost delayBounds chan msg = do
    rndDrop <- randomIO
    rndDelay <- randomRIO delayBounds

    unless (rndDrop < lost) $ void $ forkIO $ do
        threadDelay rndDelay
        atomically $ TChan.writeTChan chan msg

-- | Read a value from a `Chan', rather unreliable
readChan :: Double  -- ^ Message loss probability
         -> (Int, Int)  -- ^ Message acceptance delay bounds (in microseconds)
         -> Chan a  -- ^ `Chan' to read from
         -> IO a
readChan lost delayBounds chan = do
    rndDrop <- randomIO
    rndDelay <- randomRIO delayBounds

    msg <- atomically $ TChan.readTChan chan

    if rndDrop < lost
        then readChan lost delayBounds chan
        else do
            threadDelay rndDelay
            return msg
