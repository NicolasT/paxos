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

module Network.Paxos.Synod.Proposer (
    -- * Proposer functionality
      ProposerState
    , Action(..)
    , startRound
    , handlePromise

    -- * Re-exports of useful ProposalId functions
    , ProposalId
    , initialProposalId
    , succProposalId

    -- * Testing
    , tests
    ) where

import Control.Applicative

import Data.Maybe (isNothing)

import Data.Word (Word32)
import Data.Serialize
import Data.Serialize.QuickCheck

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Arbitrary, arbitrary)

import Network.Paxos.Synod.Action
import Network.Paxos.Synod.Types hiding (quorum, tests)
import Network.Paxos.Synod.Messages hiding (tests)

-- | State of a Proposer
data ProposerState nodeId value = ProposerState { proposalId :: ProposalId nodeId
                                                , quorum :: Quorum
                                                , value :: value
                                                , acceptors :: [nodeId]
                                                , highestAccepted :: Maybe (AcceptedValue nodeId value)
                                                }
  deriving (Show, Eq)

serial :: Word32
serial = 0x20121213

instance (Serialize nodeId, Serialize value) => Serialize (ProposerState nodeId value) where
    get = do
        serial' <- getWord32le
        if serial' /= serial
            then fail "ProposerState: invalid serial"
            else ProposerState <$> get <*> get <*> get <*> get <*> get
    put (ProposerState a b c d e) = putWord32le serial >> put a >> put b >> put c >> put d >> put e

instance (Arbitrary nodeId, Arbitrary value) => Arbitrary (ProposerState nodeId value) where
    arbitrary = ProposerState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


-- | Start a single round using given quorum, `ProposalId' and value to propose
startRound :: Quorum  -- ^ Quorum size
           -> ProposalId nodeId  -- ^ `ProposalId' to use
           -> value  -- ^ Value to propose
           -> (ProposerState nodeId value, [Action nodeId value])
startRound quorum' proposalId' value' = (state, [msg])
  where
    state = ProposerState { proposalId = proposalId'
                          , quorum = quorum'
                          , value = value'
                          , acceptors = []
                          , highestAccepted = Nothing
                          }
    msg = Broadcast Acceptors $ MsgPrepare $ Prepare proposalId'

prop_startRound1 :: Quorum -> ProposalId Int -> () -> Bool
prop_startRound1 q p v = and [ proposalId s == p
                             , quorum s == q
                             , value s == v
                             , null $ acceptors s
                             , isNothing $ highestAccepted s
                             ]
  where
    (s, _) = startRound q p v

prop_startRound2 :: Quorum -> ProposalId Int -> () -> Bool
prop_startRound2 q p v = actions == [Broadcast Acceptors $ MsgPrepare $ Prepare p]
  where
    (_, actions) = startRound q p v


-- | Handle a single `Promise' message received from an Acceptor
handlePromise :: Ord nodeId => ProposerState nodeId value  -- ^ Current state
                            -> nodeId  -- ^ Identifier of the node from which the message was received
                            -> Promise nodeId value  -- ^ Received message
                            -> (ProposerState nodeId value, [Action nodeId value])
handlePromise state acceptor (Promise proposalId' highestAccepted')
    -- Promises for older proposals than the one we're handling are ignored
    -- (most likely some message reordering occurred)
    | proposalId' < proposalId state = (state, [])
    -- Promises of proposals newer than the one we're handling are ignored,
    -- although we might want to send a hint to the manager he might want
    -- to restart a round with a higher proposal number
    | proposalId' > proposalId state = (state, []) -- TODO Give up and start new round?
    -- The given proposal number matches the one this proposer is handling
    | otherwise =
        -- Check whether we already handled a promise of this acceptor
        if acceptor `elem` acceptors state
            -- If so, ignore the message (some message duplication occurred)
            then (state, [])
            -- All well, update state and return some actions (if any)
            else (state', msgs)
  where
    -- Updated state assuming
    -- * The message is a promise matching the proposal number we're
    --   handling
    -- * A promise of this acceptor wasn't handled yet
    state' = state { acceptors = acceptor : acceptors state
                   , highestAccepted = selectedAccepted
                   }
    -- Select the `AcceptedValue' to remember: this is the maximum of the
    -- one we received before (if any) and the one contained in this
    -- message (again, if any)
    selectedAccepted = case highestAccepted state of
                           Nothing -> highestAccepted'
                           Just v -> case highestAccepted' of
                               Nothing -> Just v
                               Just v' -> Just $ max v v'
    -- Actions to execute as a result of this state change
    -- If we reached (but didn't exceed) the quorum (i.e. a quorum of
    -- acceptors sent a promise for the current proposal), send an `Accept'
    -- message to all Acceptors. The value contained in this command should
    -- be the one of the highest `AcceptedValue' we received in any
    -- `Promise', if any. Otherwise, we can use any value we want (or, more
    -- likely, the one our user wants to distribute).
    msgs = if length (acceptors state') /= fromIntegral (unQuorum $ quorum state')
               then []
               else [Broadcast Acceptors $ MsgAccept $ Accept (proposalId state') value']
    -- Retrieve the value to be distributed in an `Accept' message. This is
    -- the value of the highest `AcceptedValue' we received as part of
    -- `Promise' message, or the value passed by our user initially if
    -- none.
    value' = maybe (value state') (\(AcceptedValue _ v) -> v) (highestAccepted state')

prop_handlePromise :: ProposerState Int ()
                   -> Int
                   -> Promise Int ()
                   -> Bool
prop_handlePromise state acceptor p@(Promise proposalId' highestAccepted')
    | proposalId' /= proposalId state = result == (state, [])
    | otherwise =
        if acceptor `elem` acceptors state
            then result == (state, [])
            else and [ acceptor `elem` acceptors state'
                     , length (acceptors state') == length (acceptors state) + 1
                     , highestAccepted state' == max (highestAccepted state) highestAccepted'
                     , proposalId state' == proposalId state
                     , (length (acceptors state') /= fromIntegral (unQuorum $ quorum state')) ||
                           (actions == [Broadcast Acceptors $ MsgAccept $ Accept (proposalId state') value'])
                     ]
  where
    result@(state', actions) = handlePromise state acceptor p
    value' = maybe (value state') (\(AcceptedValue _ v) -> v) (highestAccepted state')


-- | Tests
tests :: Test
tests = testGroup "Network.Paxos.Synod.Proposer" [
              testProperty "startRound1" prop_startRound1
            , testProperty "startRound2" prop_startRound2
            , testProperty "handlePromise" prop_handlePromise
            , testProperty "ProposerState Seralize" $ prop_serialize (undefined :: ProposerState String Int)
            ]
