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

{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Network.Paxos.Lease
    (
      -- * The $PaxosLease$ monad
      PaxosLease
    , runPaxosLease
    , State
    , createState

      -- * Actions
    , Action(..)
    , propose
    , hasLease
    , handleMessage

      -- * Misc
    , version
    ) where

import Prelude hiding (catch)

import Data.Word (Word8)
import Data.Binary (Binary, getWord8)
import qualified Data.Binary as B
import Control.Monad (liftM, liftM2)

import Control.Exception (assert)

import qualified Control.Monad.State as S
import Control.Monad.Trans (MonadIO, liftIO)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)

import System.Log.Logger.TH (deriveLoggers)
import qualified System.Log.Logger as HSL
$(deriveLoggers "HSL" [HSL.DEBUG, HSL.INFO])

version :: (Int, Int, Int)
version = (0, 0, 1)

type BallotNumber = Int
type Id = Int

data Proposal = Empty
              | Proposal Int
  deriving (Show, Eq)

instance Binary Proposal where
    put Empty = B.put (0 :: Word8)
    put (Proposal t) = do
        B.put (1 :: Word8)
        B.put t

    get = do
        tag <- getWord8
        case tag of
            0 -> return Empty
            1 -> liftM Proposal B.get
            _ -> error "Invalid proposal tag"


data ProtocolMessage = PrepareRequest BallotNumber
                     | ProposeRequest BallotNumber Proposal
                     | PrepareResponse BallotNumber Proposal
                     | ProposeResponse BallotNumber
  deriving (Show, Eq)

instance Binary ProtocolMessage where
    put (PrepareRequest i) = do
        B.put (0 :: Word8)
        B.put i
    put (ProposeRequest i p) = do
        B.put (1 :: Word8)
        B.put i
        B.put p
    put (PrepareResponse i p) = do
        B.put (2 :: Word8)
        B.put i
        B.put p
    put (ProposeResponse i) = do
        B.put (3 :: Word8)
        B.put i

    get = do
        tag <- getWord8
        case tag of
            0 -> liftM PrepareRequest B.get
            1 -> liftM2 ProposeRequest B.get B.get
            2 -> liftM2 PrepareResponse B.get B.get
            3 -> liftM ProposeResponse B.get
            _ -> error "Invalid message tag"


newtype PaxosLease a = PL {
    runPL :: S.StateT State IO a
}
  deriving (Monad, MonadIO, S.MonadState State)

runPaxosLease :: PaxosLease a -> State -> IO (a, State)
runPaxosLease = S.runStateT . runPL

data State = State
    { stId :: Id

    -- Acceptor data
    , stHighestPromised :: BallotNumber
    , stAcceptedProposal :: MVar Proposal

    -- Proposer data
    , stBallotNumber :: MVar BallotNumber
    , stHasLease :: MVar Bool
    , stNumPeers :: Int
    , stNumOpen :: Int
    , stNumAccepted :: Int
    }

instance Show State where
    show s = "State " ++
             "{ stId = " ++ a1 ++
             ", stHighestPromised = " ++ a2 ++
             ", stNumPeers = " ++ a3 ++
             ", stNumOpen = " ++ a4 ++
             ", stNumAccepted = " ++ a5 ++
             " }"
      where a1 = show $ stId s
            a2 = show $ stHighestPromised s
            a3 = show $ stNumPeers s
            a4 = show $ stNumOpen s
            a5 = show $ stNumAccepted s

createState :: Int -> Int -> IO State
createState i n = do
    a <- newMVar Empty
    b <- newMVar undefined
    h <- newMVar False

    return State { stId = i
                 , stHighestPromised = -1
                 , stAcceptedProposal = a
                 , stBallotNumber = b
                 , stHasLease = h
                 , stNumPeers = n
                 , stNumOpen = 0
                 , stNumAccepted = 0
                 }

data Action = Broadcast ProtocolMessage
            | Reply ProtocolMessage
            | Ignore
  deriving (Show, Eq)

get :: PaxosLease State
get = do
    state <- S.get
    debugM $ "Retrieved state: " ++ show state
    return state
{-# INLINE get #-}

put :: State -> PaxosLease ()
put s = do
    debugM $ "Commit state: " ++ show s
    S.put s
{-# INLINE put #-}

nextBallotNumber :: State -> BallotNumber
nextBallotNumber s = assert (n > h) n
  where h = stHighestPromised s
        i = stId s
        p = stNumPeers s
        l = p + 1
        d = h `div` l
        n = (d + 1) * l + i

f :: (Monad m) => m a -> m ()
f a = do
    _ <- a
    return ()
{-# INLINE f #-}

propose :: PaxosLease Action
propose = do
    state <- get
    let n = nextBallotNumber state
    f $ liftIO $ swapMVar (stBallotNumber state) n
    S.put $ state { stNumOpen = 0
                  , stNumAccepted = 0
                  }
    return $ Broadcast $ PrepareRequest n

hasLease :: PaxosLease Bool
hasLease = do
    state <- get
    liftIO $ readMVar $ stHasLease state


scheduleTimeout :: Int -> IO () -> IO ()
scheduleTimeout t a = f $ forkIO $ do
    threadDelay t
    a

handleMessage :: ProtocolMessage -> PaxosLease Action
handleMessage (PrepareRequest i) = do
    state <- get
    if i < stHighestPromised state
        then return Ignore
        else do
            put $ state { stHighestPromised = i }
            a <- liftIO $ readMVar $ stAcceptedProposal state
            return . Reply $ PrepareResponse i a

handleMessage (ProposeRequest i p@(Proposal t)) = do
    state <- get
    if i < stHighestPromised state
        then return Ignore
        else do
            f $ liftIO $ swapMVar (stAcceptedProposal state) p
            f $ liftIO $ scheduleTimeout t $
                f $ swapMVar (stAcceptedProposal state) Empty
            return . Reply $ ProposeResponse i

handleMessage (ProposeRequest _ Empty) =
    error "Received Propose request with Empty proposal"

handleMessage (PrepareResponse i p) = do
    state <- get
    b <- liftIO $ readMVar $ stBallotNumber state
    if i /= b
        then return Ignore
        else do
            case p of
                Empty -> do
                    let n = stNumOpen state + 1
                    put $ state { stNumOpen = n }
                _ -> return ()

            state' <- get
            let a = stNumOpen state'
            if a < (stNumPeers state' `div` 2) + 1
                then return Ignore
                else do
                    liftIO $ scheduleTimeout timeout $ do
                        f $ swapMVar (stBallotNumber state') undefined
                        f $ swapMVar (stHasLease state') False
                    return . Broadcast $ ProposeRequest b $
                        Proposal timeout

handleMessage (ProposeResponse i) = do
    state <- get
    b <- liftIO $ readMVar $ stBallotNumber state
    if i /= b
        then return Ignore
        else do
            let a = stNumAccepted state + 1
            put $ state { stNumAccepted = a }
            if a < (stNumPeers state `div` 2) + 1
                then return Ignore
                else do
                    f $ liftIO $ swapMVar (stHasLease state) True
                    infoM "*** I'm the leader"
                    return Ignore

timeout :: Int
timeout = 10000
