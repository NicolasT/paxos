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

import Control.Concurrent hiding (readChan, writeChan)
import Control.Concurrent.STM

import qualified Control.Concurrent.UnreliableChan as U

import System.Random

import System.IO (stderr)

import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Formatter

import Network.Paxos.Synod
import qualified Network.Paxos.Synod.Proposer as P
import qualified Network.Paxos.Synod.Acceptor as A
import qualified Network.Paxos.Synod.Learner as L

type NodeId = String
type Value = String
type Actions = [Action NodeId Value]
type NetworkChannel = TChan (NodeId, Action NodeId Value)
type MessageChannel = TChan (NodeId, Message NodeId Value)

lost :: Double
lost = 0.02
minDelay :: Int
minDelay = 1000
maxDelay :: Int
maxDelay = 300000
readChan :: U.Chan a -> IO a
readChan = U.readChan lost (minDelay, maxDelay)
writeChan :: U.Chan a -> a -> IO ()
writeChan = U.writeChan lost (minDelay, maxDelay)

handleActions :: NodeId -> NetworkChannel -> Actions -> IO ()
handleActions name network = mapM_ (\a -> writeChan network (name, a))

runProposer :: MVar Value -> String -> Quorum -> P.ProposalId NodeId -> Value -> MessageChannel -> NetworkChannel -> IO ()
runProposer lock name q p v chan network = do
    info $ "Running proposer for proposal " ++ show p
    myThreadId >>= void . forkIO . runWatcher

    handleActions name network actions0
    debug $ "Initial actions: " ++ show actions0

    loop state0
  where
    (state0, actions0) = P.startRound q p v
    loop state = do
        (sender, msg) <- readChan chan
        debug $ "Received message from '" ++ sender ++ "': " ++ show msg
        case msg of
            MsgPromise m -> do
                let (state', actions) = P.handlePromise state sender m
                debug $ "Actions: " ++ show actions
                handleActions name network actions
                loop state'
            _ -> loop state

    timeoutBounds = (0, 800000)
    runWatcher tid = do
        -- If no value is chosen in this timeframe, start a new round
        threadDelay 800000
        decission <- tryTakeMVar lock
        case decission of
            Nothing -> do
                -- Some random delay to give other proposers a chance
                randomRIO timeoutBounds >>= threadDelay
                killThread tid
                runProposer lock name q (P.succProposalId p) v chan network
            Just _ ->
                info "Learner learned a value, all done"

    debug = debugM name
    info = infoM name

runAcceptor :: Int -> MessageChannel -> NetworkChannel -> IO ()
runAcceptor i chan network = loop state0
  where
    state0 = A.initialize
    loop state = do
        (sender, msg) <- readChan chan
        debug $ "Received message from '" ++ sender ++ "': " ++ show msg
        case msg of
            MsgPrepare m -> do
                let (state', actions) = A.handlePrepare state sender m
                debug $ "Actions: " ++ show actions
                handleActions name network actions
                loop state'
            MsgAccept m -> do
                let (state', actions) = A.handleAccept state m
                debug $ "Actions: " ++ show actions
                handleActions name network actions
                loop state'
            _ -> loop state

    name = "acceptor" ++ show i
    debug = debugM name

runLearner :: Int -> Quorum -> MessageChannel -> NetworkChannel -> MVar Value -> IO ()
runLearner i q chan _network lock = loop state0
  where
    state0 = L.initialize q
    loop state = do
        (sender, msg) <- readChan chan
        debug $ "Received message from '" ++ sender ++ "': " ++ show msg
        case msg of
            MsgAccepted m -> do
                let state' = L.handleAccepted state sender m
                case L.getValue state' of
                    Nothing -> loop state'
                    Just v -> do
                        info $ "Learned value: " ++ show v
                        putMVar lock v
            _ -> loop state

    name = "learner" ++ show i
    debug = debugM name
    info = infoM name

runNetwork :: NetworkChannel -> [(String, MessageChannel)] -> MessageChannel -> MessageChannel -> IO ()
runNetwork chan proposers acceptors learners = forever loop
  where
    loop = do
        (sender, action) <- atomically $ readTChan chan
        atomically $ case action of
            Send target message ->
                case lookup target proposers of
                    Nothing -> error $ "Unknown target '" ++ target ++ "'"
                    Just pchan -> writeTChan pchan (sender, message)
            Broadcast group message -> case group of
                                          Acceptors -> writeTChan acceptors (sender, message)
                                          Learners -> writeTChan learners (sender, message)
                                          

main :: IO ()
main = do
    handler <- do
        h <- streamHandler stderr DEBUG
        return $ setFormatter h (simpleLogFormatter "[$loggername] $msg")
    updateGlobalLogger rootLoggerName $
        setLevel DEBUG . setHandlers [handler]

    proposerChans <- replicateM numProposers newTChanIO
    acceptorsChan <- newBroadcastTChanIO
    learnersChan <- newBroadcastTChanIO

    network <- newTChanIO

    let proposers = [("proposer" ++ show i, chan) | (i, chan) <- zip [(0 :: Int) ..] proposerChans]

    lock <- newEmptyMVar

    networkHandler <- forkIO $ runNetwork network proposers acceptorsChan learnersChan

    learners <- forM [0 .. numLearners - 1] $ \i -> do
                    chan <- atomically $ dupTChan learnersChan
                    forkIO $ runLearner i q chan network lock

    acceptors <- forM [0 .. numAcceptors - 1] $ \i -> do
                    chan <- atomically $ dupTChan acceptorsChan
                    forkIO $ runAcceptor i chan network


    forM_ proposers $ \(name, chan) -> do
        timeout <- randomRIO (500, 10000)
        threadDelay timeout
        let msg = "Hello world, from " ++ name ++ "!"
        void $ forkIO $ runProposer lock name q (P.initialProposalId name) msg chan network

    result <- takeMVar lock

    mapM_ killThread acceptors
    mapM_ killThread learners
    killThread networkHandler

    putStrLn $ "Learned value: " ++ result

  where
    numLearners, numAcceptors, numProposers :: Int
    numLearners = 2
    numAcceptors = 5
    numProposers = 2
    q = quorum $ numAcceptors `div` 2 + 1
