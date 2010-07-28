{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Paxos.Lease where

-- import Data.Int (Int32)
import Data.Word (Word8)
import Data.Binary (Binary, getWord8)
import qualified Data.Binary as B
import Control.Monad (liftM, liftM2)
import Network.Socket (Socket)

import Control.Monad.Trans (MonadIO)
import Control.Monad.State (StateT, MonadState, get, put)

version :: (Int, Int, Int)
version = (0, 0, 1)

type BallotNumber = Int
type Id = Int

data Proposal = Empty
              | Proposal { prProposerId :: Id
                         , prTimeout :: Int
                         }
    deriving (Show, Eq)

instance Binary Proposal where
    put Empty = B.put (0 :: Word8)
    put (Proposal i t) = do B.put (1 :: Word8)
                            B.put i
                            B.put t

    get = do tag <- getWord8
             case tag of
                 0 -> return Empty
                 1 -> liftM2 Proposal B.get B.get
                 _ -> error "Invalid proposal tag"


data Request = PrepareRequest BallotNumber
             | ProposeRequest BallotNumber Proposal
    deriving (Show, Eq)

instance Binary Request where
    put (PrepareRequest i) = do B.put (0 :: Word8)
                                B.put i
    put (ProposeRequest i p) = do B.put (1 :: Word8)
                                  B.put i
                                  B.put p

    get = do tag <- getWord8
             case tag of
                 0 -> liftM PrepareRequest B.get
                 1 -> liftM2 ProposeRequest B.get B.get
                 _ -> error "Invalid request tag"


data Response = PrepareResponse BallotNumber Proposal
              | ProposeResponse BallotNumber
    deriving (Show, Eq)

instance Binary Response where
    put (PrepareResponse i p) = do B.put (0 :: Word8)
                                   B.put i
                                   B.put p
    put (ProposeResponse i) = do B.put (1 :: Word8)
                                 B.put i

    get = do tag <- getWord8
             case tag of
                 0 -> liftM2 PrepareResponse B.get B.get
                 1 -> liftM ProposeResponse B.get
                 _ -> error "Invalid response tag"


data State = State { stId :: Int
                   , stHighestPromised :: BallotNumber
                   , stBallotNumber :: BallotNumber
                   , stIsOwner :: Bool
                   , stAcceptedProposal :: Proposal

                   , stPeers :: [Socket]
                   }
    deriving (Show, Eq)


newtype PaxosLease a = PaxosL {
    runPaxosL :: StateT State IO a
} deriving (Monad, MonadIO, MonadState State)

broadcast :: Request -> PaxosLease ()
broadcast = error "Not implemented"

send :: a -> Response -> PaxosLease ()
send _ _ = error "Not implemented"

nextBallotNumber :: PaxosLease BallotNumber
nextBallotNumber = do state <- get
                      let h = stHighestPromised state
                          i = stId state
                          p = stPeers state
                          l = length p + 1
                          d = h `div` l
                      return $ (d + 1) * l + i


propose :: PaxosLease ()
propose = do state <- get
             n <- nextBallotNumber
             put state { stBallotNumber = n }
             let r = PrepareRequest n
             broadcast r


handleRequest :: Request -> PaxosLease ()
handleRequest (PrepareRequest i) = do state <- get
                                      let h = stHighestPromised state
                                      if i < h
                                          then return ()
                                          else do put state { stHighestPromised = i }
                                                  let r = PrepareResponse i $ stAcceptedProposal state
                                                  send undefined r

handleRequest (ProposeRequest _ _) = error "Not implemented"

handleResponse :: Response -> PaxosLease ()
handleResponse (PrepareResponse i p) = do state <- get
                                          if i /= (stBallotNumber state)
                                              then return ()
                                              else error "Not implemented"
handleResponse (ProposeResponse i) = error "Not implemented"