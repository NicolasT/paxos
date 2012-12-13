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

module Network.Paxos.Synod.Acceptor (
    -- * Acceptor functionality
      AcceptorState
    , Action(..)
    , initialize
    -- ** Incoming message handlers
    , handlePrepare
    , handleAccept
    -- * Testing
    , tests
    ) where

import Data.Maybe (isNothing)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Arbitrary, arbitrary)

import Network.Paxos.Synod.Action
import Network.Paxos.Synod.Types hiding (tests)
import Network.Paxos.Synod.Messages hiding (tests)

-- | State of an Acceptor
data AcceptorState nodeId value = AcceptorState { highestPromise :: Maybe (ProposalId nodeId)
                                                , highestAccepted :: Maybe (AcceptedValue nodeId value)
                                                }
  deriving (Show, Eq)

instance (Arbitrary nodeId, Arbitrary value) => Arbitrary (AcceptorState nodeId value) where
    arbitrary = do
        promised <- arbitrary
        accepted <- case promised of
                        Nothing -> return Nothing
                        Just _ -> arbitrary
        return $ AcceptorState promised accepted


-- | Initial Acceptor state
initialize :: AcceptorState nodeId value
initialize = AcceptorState { highestPromise = Nothing
                           , highestAccepted = Nothing
                           }

prop_initialize :: Bool
prop_initialize = isNothing (highestPromise state0) && isNothing (highestAccepted state0)
  where
    state0 = initialize


-- | Handle a single `Prepare' message received from a Proposer
handlePrepare :: Ord nodeId
              => AcceptorState nodeId value  -- ^ Current state
              -> nodeId  -- ^ Identifier of the node from which the message was received
              -> Prepare nodeId  -- ^ Received message
              -> (AcceptorState nodeId value, [Action nodeId value])
handlePrepare state proposer (Prepare proposal) =
    case highestPromise state of
        -- If we didn't send any promise yet, the given proposal is OK, and we promise to store it
        -- and never accept any lower proposals
        Nothing -> (state', sendPromise)
        -- Otherwise, only accept the proposal if it's higher than the highest one for which we
        -- returned a promise before
        Just promised -> case compare promised proposal of
                            -- The highest proposal to which we send a `Promise' reply is lower
                            -- than this one, accept it
                            LT -> (state', sendPromise)
                            -- They're equal, most likely some message duplication occurred
                            EQ -> (state, [])
                            -- We already promised not to accept any proposals lower than `promised',
                            -- and the current proposal is lower, so we ignore this proposal
                            GT -> (state, []) -- TODO Nack
  where
    -- We accept this proposal: store it
    state' = state { highestPromise = Just proposal }
    -- We accept this proposal: return a `Promise' to the Proposer
    sendPromise = [Send proposer $ MsgPromise $ Promise proposal (highestAccepted state')]

prop_handlePrepare :: AcceptorState Int () -> Int -> Prepare Int -> Bool
prop_handlePrepare state proposer msg@(Prepare proposalId) =
    case highestPromise state of
        Nothing -> (highestPromise state' == Just proposalId) &&
                      (actions == [Send proposer $ MsgPromise $ Promise proposalId Nothing])
        Just promised -> (highestAccepted state' == highestAccepted state) &&
                            case compare promised proposalId of
                                LT -> (highestPromise state' == Just proposalId) &&
                                        (actions == [Send proposer $ MsgPromise $ Promise proposalId (highestAccepted state)])
                                EQ -> state' == state && null actions
                                GT -> state' == state && null actions
  where
    (state', actions) = handlePrepare state proposer msg


-- | Handle a single `Accept' message received from a Proposer
handleAccept :: Ord nodeId
             => AcceptorState nodeId value  -- ^ Current state
             -> Accept nodeId value  -- ^ Received message
             -> (AcceptorState nodeId value, [Action nodeId value])
handleAccept state (Accept proposal value) =
    case highestPromise state of
        -- We didn't promise anything yet, so we can accept this (and update our `highestPromise'
        -- value accordingly!)
        Nothing -> (state', [sendAccepted])
        -- We already sent a `Promise', check whether we can accept what's offered...
        Just promised -> case compare promised proposal of
                             -- We promised not to accept any proposals below `promised', but
                             -- this proposal exceeds this, so we can accept it (and update our
                             -- `highestPromise' value accordingly)
                             LT -> (state', [sendAccepted])
                             -- The given proposal is equal to the one we promised to use as
                             -- lower-bound. This is just fine, but we only want to broadcast an
                             -- `Accepted' message once
                             EQ -> case highestAccepted state of
                                     -- We didn't accept anything before, so update the state
                                     -- and broadcast an `Accepted' message
                                     Nothing -> (state', [sendAccepted])
                                     -- We accepted something before...
                                     Just (AcceptedValue p _) ->
                                         if p == proposal
                                             -- If we accepted the current proposal before,
                                             -- ignore this message, it's a duplicate
                                             then (state, [])
                                             -- Otherwise, we accepted an older proposal before.
                                             -- Update the state and broadcast `Accepted'
                                             else (state', [sendAccepted])
                             GT -> (state, []) -- TODO Nack
  where
    -- Assuming the conditions to accept this message are met, update the state...
    state' = state { highestPromise = Just proposal
                   , highestAccepted = Just $ AcceptedValue proposal value
                   }
    -- ... and broadcast an `Accepted' message to all Learners
    sendAccepted = Broadcast Learners $ MsgAccepted $ Accepted proposal value

prop_handleAccept :: AcceptorState Int () -> Accept Int () -> Bool
prop_handleAccept state msg@(Accept proposal value) =
    case highestPromise state of
        Nothing -> (state' == acceptedState) && (actions == [broadcastAccepted])
        Just promised -> case compare promised proposal of
                             LT -> (state' == acceptedState) && (actions == [broadcastAccepted])
                             EQ -> case highestAccepted state of
                                       Nothing -> (state' == acceptedState) && (actions == [broadcastAccepted])
                                       Just (AcceptedValue p _) ->
                                           if p == proposal
                                               then (state' == state) && null actions
                                               else (state' == acceptedState) && (actions == [broadcastAccepted])
                             GT -> (state' == state) && null actions
  where
    (state', actions) = handleAccept state msg
    acceptedState = state { highestPromise = Just proposal
                          , highestAccepted = Just $ AcceptedValue proposal value
                          }
    broadcastAccepted = Broadcast Learners $ MsgAccepted $ Accepted proposal value


-- | Tests
tests :: Test
tests = testGroup "Network.Paxos.Synod.Acceptor" [
              testProperty "initialize" prop_initialize
            , testProperty "handlePrepare" prop_handlePrepare
            , testProperty "handleAccept" prop_handleAccept
            ]
