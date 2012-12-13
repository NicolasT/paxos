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

module Main (main) where

import Control.Monad
import Control.Applicative

import Control.Concurrent
import Control.Concurrent.STM

import System.Random

import Network.Paxos.Synod
import qualified Network.Paxos.Synod.Proposer as P
import qualified Network.Paxos.Synod.Acceptor as A
import qualified Network.Paxos.Synod.Learner as L

-- TODO Use System.Logger or something alike and log inside run* actions
-- TODO Implement lossy and duplicating channels (and update code
-- accordingly: restart a new round after a certain timeout etc)

type NodeId = String
type Value = String
type Actions = [Action NodeId Value]
type NetworkChannel = TChan (NodeId, Action NodeId Value)
type MessageChannel = TChan (NodeId, Message NodeId Value)

-- | A version of readTChan which adds some random delay before returning
-- the actual message
readTChan' :: TChan a -> IO a
readTChan' chan = do
    delay <- (`rem` 2000000) <$> randomIO
    threadDelay delay
    atomically $ readTChan chan

handleActions :: NodeId -> NetworkChannel -> Actions -> IO ()
handleActions name network = atomically . mapM_ (\a -> writeTChan network (name, a))

runProposer :: Quorum -> P.ProposalId NodeId -> Value -> MessageChannel -> NetworkChannel -> IO ()
runProposer q p v chan network = do
    handleActions "proposer" network actions0
    loop state0
  where
    (state0, actions0) = P.startRound q p v
    loop state = do
        (sender, msg) <- atomically $ readTChan chan
        case msg of
            MsgPromise m -> do
                let (state', actions) = P.handlePromise state sender m
                handleActions "proposer" network actions
                loop state'
            _ -> loop state

runAcceptor :: Int -> MessageChannel -> NetworkChannel -> IO ()
runAcceptor i chan network = loop state0
  where
    state0 = A.initialize
    loop state = do
        (sender, msg) <- readTChan' chan
        case msg of
            MsgPrepare m -> do
                let (state', actions) = A.handlePrepare state sender m
                handleActions ("acceptor" ++ show i) network actions
                loop state'
            MsgAccept m -> do
                let (state', actions) = A.handleAccept state m
                handleActions ("acceptor" ++ show i) network actions
                loop state'
            _ -> loop state

runLearner :: Int -> Quorum -> MessageChannel -> NetworkChannel -> MVar Value -> IO ()
runLearner _i q chan _network lock = loop state0
  where
    state0 = L.initialize q
    loop state = do
        (sender, msg) <- readTChan' chan
        case msg of
            MsgAccepted m -> do
                let state' = L.handleAccepted state sender m
                case L.getValue state' of
                    Nothing -> loop state'
                    Just v -> putMVar lock v
            _ -> loop state

runNetwork :: NetworkChannel -> MessageChannel -> MessageChannel -> MessageChannel -> IO ()
runNetwork chan proposer acceptors learners = forever loop
  where
    loop = do
        (sender, action) <- atomically $ readTChan chan
        putStrLn $ sender ++ ": " ++ show action
        case action of
            Send target message -> if target == "proposer"
                                      then atomically $ writeTChan proposer (sender, message)
                                      else error $ "Unknown target '" ++ target ++ "'"
            Broadcast group message -> case group of
                                          Acceptors -> atomically $ writeTChan acceptors (sender, message)
                                          Learners -> atomically $ writeTChan learners (sender, message)
                                          

main :: IO ()
main = do
    proposerChan <- newTChanIO
    acceptorsChan <- newBroadcastTChanIO
    learnersChan <- newBroadcastTChanIO

    network <- newTChanIO

    lock <- newEmptyMVar

    networkHandler <- forkIO $ runNetwork network proposerChan acceptorsChan learnersChan

    learners <- forM [0 .. numLearners - 1] $ \i -> do
                    chan <- atomically $ dupTChan learnersChan
                    forkIO $ runLearner i q chan network lock

    acceptors <- forM [0 .. numAcceptors - 1] $ \i -> do
                    chan <- atomically $ dupTChan acceptorsChan
                    forkIO $ runAcceptor i chan network

    proposer <- forkIO $ runProposer q (P.initialProposalId "proposer") "Hello, world!" proposerChan network

    result <- takeMVar lock

    mapM_ killThread acceptors
    mapM_ killThread learners
    killThread proposer
    killThread networkHandler

    putStrLn $ "Learned value: " ++ result

  where
    numLearners, numAcceptors :: Int
    numLearners = 2
    numAcceptors = 5
    q = quorum $ numAcceptors `div` 2 + 1
