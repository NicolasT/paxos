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

module Network.Paxos.Synod.Learner (
    -- * Learner functionality
      LearnerState
    , initialize
    -- ** Incoming message handlers
    , handleAccepted
    -- ** Result extraction
    , getValue
    -- * Testing
    , tests
    ) where

import Control.Applicative

import Data.Maybe (fromJust, isNothing)

import Data.Map (Map)
import qualified Data.Map as Map

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Arbitrary, arbitrary, oneof)

import Network.Paxos.Synod.Types hiding (quorum, tests)
import Network.Paxos.Synod.Messages hiding (tests)

-- | State of a Learner
data LearnerState nodeId value = Learning { quorum :: Quorum
                                          , highestAccepted :: Map nodeId (ProposalId nodeId)
                                          , accepted :: Map (ProposalId nodeId) [nodeId]
                                          }
                               | Decided value
  deriving (Show, Eq)

instance (Ord nodeId, Arbitrary nodeId, Arbitrary value) => Arbitrary (LearnerState nodeId value) where
    arbitrary = oneof [learning, decided]
      where
        decided = Decided <$> arbitrary
        learning = do
            state0 <- initialize <$> arbitrary
            mods <- arbitrary
            let f (nodeId, msg) s = case handleAccepted s nodeId msg of
                                        Decided _ -> s
                                        s'@Learning{} -> s'
                state = foldr f state0 mods
            return state


-- | Generate an initial Learner state
initialize :: Quorum  -- ^ Number of nodes which form a quorum
           -> LearnerState nodeId value
initialize quorum' = Learning { quorum = quorum'
                              , highestAccepted = Map.empty
                              , accepted = Map.empty
                              }

prop_initialize :: Quorum -> Bool
prop_initialize quorum' = and [ isNothing $ getValue state
                              , quorum state == quorum'
                              , Map.null $ highestAccepted state
                              , Map.null $ accepted state
                              ]
  where
    state = initialize quorum'


-- | Handle a single Accepted message received from an Acceptor
handleAccepted :: Ord nodeId
               => LearnerState nodeId value  -- ^ Current state
               -> nodeId  -- ^ Identifier of the node from which the message was received
               -> Accepted nodeId value  -- ^ Received message
               -> LearnerState nodeId value
handleAccepted state acceptor (Accepted proposalId value) =
    case state of
        Decided _ -> state
        Learning{} ->
            if acceptor `Map.member` highestAccepted state
                then if highestProposalId >= proposalId
                    then state
                    else handleMember state
                else handleNotMember state
  where
    Just highestProposalId = Map.lookup acceptor (highestAccepted state)

    listToMaybe l
        | null l = Nothing
        | otherwise = Just l

    handleMember s = handleNotMember $ s { accepted = Map.update (listToMaybe . filter (/= acceptor)) highestProposalId (accepted s) }

    -- TODO This is plain ugly
    handleNotMember s = check proposalId $ s { highestAccepted = Map.insert acceptor proposalId (highestAccepted s)
                                             , accepted = Map.adjust ((:) acceptor) proposalId (accepted s `Map.union` Map.singleton proposalId [])
                                             }

    check p s =
        if maybe 0 length (Map.lookup p (accepted s)) == fromIntegral (unQuorum (quorum s))
            then Decided value
            else s

prop_handleAccepted1 :: LearnerState Int () -> Int -> Accepted Int () -> Bool
prop_handleAccepted1 state acceptor msg@(Accepted proposalId _) =
    isDecided || (fromJust (Map.lookup acceptor (highestAccepted state')) >= proposalId)
  where
    state' = handleAccepted state acceptor msg
    isDecided = case state' of
                    Decided _ -> True
                    Learning{} -> False

prop_handleAccepted2 :: LearnerState Int () -> Int -> Accepted Int () -> Bool
prop_handleAccepted2 state acceptor msg
    | isDecided = True
    | otherwise = acceptor `elem` list
  where
    state' = handleAccepted state acceptor msg
    isDecided = case state' of
                    Decided _ -> True
                    Learning{} -> False
    Just p = Map.lookup acceptor (highestAccepted state')
    Just list = Map.lookup p (accepted state')

-- TODO Add property/test to check `Decided' is returned once quorum is
-- reached


-- | Extract the learned value from the Learner state, if any.
--
-- Once a value has been learned, handling more `Prepare' messages becomes
-- a no-op.
getValue :: LearnerState nodeId value -> Maybe value
getValue state = case state of
    Learning{} -> Nothing
    Decided value -> Just value

prop_getValue :: LearnerState Int Int -> Bool
prop_getValue state = case state of
    Decided value -> getValue state == Just value
    Learning{} -> isNothing $ getValue state


-- | Tests
tests :: Test
tests = testGroup "Network.Paxos.Synod.Learner" [
              testProperty "initialize" prop_initialize
            , testProperty "handleAccepted1" prop_handleAccepted1
            , testProperty "handleAccepted2" prop_handleAccepted2
            , testProperty "prop_getValue" prop_getValue
            ]
