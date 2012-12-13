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

{-# LANGUAGE DeriveDataTypeable #-}

module Network.Paxos.Synod.Types (
    -- * Proposal identifier handling
      ProposalId
    , initialProposalId
    , succProposalId
    , bumpProposalId

    -- * Utilities
    , Quorum(unQuorum)
    , quorum
    , AcceptedValue(..)

    -- * Testing
    , tests
    ) where

import Control.Applicative ((<$>), (<*>))

import Data.Word (Word, Word64)

import Data.Serialize
import Data.Typeable

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Arbitrary, arbitrary)

-- | Representation of a proposal identifier
data ProposalId nodeId = ProposalId {-# UNPACK #-} !Word64 nodeId
  deriving (Show, Eq, Typeable)

-- Note: even though the following instance should be exactly the one
-- derived using `deriving(Ord)', I prefer to provide an explicit
-- implementation
instance Ord nodeId => Ord (ProposalId nodeId) where
    compare (ProposalId i1 n1) (ProposalId i2 n2)
        | i1 /= i2 = compare i1 i2
        | otherwise = compare n1 n2

instance Serialize nodeId => Serialize (ProposalId nodeId) where
    get = ProposalId <$> get <*> get
    put (ProposalId i nodeId) = put i >> put nodeId

instance Arbitrary a => Arbitrary (ProposalId a) where
    arbitrary = ProposalId <$> arbitrary <*> arbitrary

prop_ProposalId_Ord :: ProposalId Int -> ProposalId Int -> Bool
prop_ProposalId_Ord p1@(ProposalId i1 n1) p2@(ProposalId i2 n2)
    | i1 > i2 = p1 > p2
    | i1 < i2 = p1 < p2
    | otherwise = compare p1 p2 == compare n1 n2

-- | Generate the initial (lowest) 'ProposalId' the given node will ever
-- use
initialProposalId :: nodeId  -- ^ Identifier of the node which will send the proposal
                  -> ProposalId nodeId
initialProposalId = ProposalId 1

-- | Calculate a new 'ProposalId' which will be greater than the given one,
-- retaining the node identifier
succProposalId :: Ord nodeId
               => ProposalId nodeId  -- ^ Proposal to increment
               -> ProposalId nodeId
succProposalId p = bumpProposalId p p

prop_succProposalId1 :: ProposalId Int -> Bool
prop_succProposalId1 p = succProposalId p > p

prop_succProposalId2 :: ProposalId Int -> Bool
prop_succProposalId2 p = succProposalId p < succProposalId (succProposalId p)

-- | Bump a proposal so it becomes greater than another one, e.g. to
-- restart a round with a higher 'ProposalId' based on `Nack' messages
bumpProposalId :: Ord nodeId
               => ProposalId nodeId  -- ^ Proposal to bump
               -> ProposalId nodeId  -- ^ Proposal to exceed
               -> ProposalId nodeId
bumpProposalId p1@(ProposalId _ nodeId) p2@(ProposalId i _)
    | p1 > p2 = p1
    | otherwise = ProposalId (i + 1) nodeId

prop_bumpProposalId1 :: ProposalId Int -> ProposalId Int -> Bool
prop_bumpProposalId1 p1 p2 = bumpProposalId p1 p2 > p2

prop_bumpProposalId2 :: ProposalId Int -> ProposalId Int -> Bool
prop_bumpProposalId2 p1 p2 = p2' > p1'
  where
    p1' = bumpProposalId p1 p2
    p2' = bumpProposalId p2 p1'

-- | Quorum number
newtype Quorum = Quorum { unQuorum :: Word }
  deriving (Show, Eq, Ord, Typeable)

instance Arbitrary Quorum where
    arbitrary = Quorum <$> arbitrary

-- | Smart constructor for `Quorum' values
quorum :: (Integral a, Ord a) => a -> Quorum
quorum q
    | q < 0 = error "quorum: Negative value"
    | toInteger q > toInteger (maxBound :: Word) = error "quorum: Overflow"
    | otherwise = Quorum $ fromIntegral q


-- | Representation of something accepted by an acceptor
data AcceptedValue nodeId value = AcceptedValue { acceptedProposalId :: ProposalId nodeId
                                                , acceptedValue :: value
                                                }
  deriving (Show, Typeable)

-- Explicit Eq and Ord instances so there's no (unnecessary) Eq or Ord
-- constraint on `value', since these should always be equal if the
-- corresponding `ProposalId' is equal, and useless for comparison
instance Eq nodeId => Eq (AcceptedValue nodeId value) where
    a == b = acceptedProposalId a == acceptedProposalId b

instance Ord nodeId => Ord (AcceptedValue nodeId value) where
    compare a b = compare (acceptedProposalId a) (acceptedProposalId b)

instance (Serialize nodeId, Serialize value) => Serialize (AcceptedValue nodeId value) where
    get = AcceptedValue <$> get <*> get
    put (AcceptedValue p v) = put p >> put v

instance (Arbitrary nodeId, Arbitrary value) => Arbitrary (AcceptedValue nodeId value) where
    arbitrary = AcceptedValue <$> arbitrary <*> arbitrary


prop_AcceptedValue_Eq :: AcceptedValue Int () -> AcceptedValue Int () -> Bool
prop_AcceptedValue_Eq a1 a2 =
    (a1 == a2) == (acceptedProposalId a1 == acceptedProposalId a2)

prop_AcceptedValue_Ord :: AcceptedValue Int () -> AcceptedValue Int () -> Bool
prop_AcceptedValue_Ord a1 a2 =
    compare a1 a2 == compare (acceptedProposalId a1) (acceptedProposalId a2)


-- | Tests
tests :: Test
tests = testGroup "Network.Paxos.Synod.Types" [
            -- ProposalId
              testProperty "ProposalId Ord" prop_ProposalId_Ord
            -- succProposalId
            , testProperty "succProposalId1" prop_succProposalId1
            , testProperty "succProposalId2" prop_succProposalId2
            -- bumpProposalId
            , testProperty "bumpProposalId1" prop_bumpProposalId1
            , testProperty "bumpProposalId2" prop_bumpProposalId2
            -- AcceptedValue
            , testProperty "AcceptedValue Eq" prop_AcceptedValue_Eq
            , testProperty "AcceptedValue Ord" prop_AcceptedValue_Ord
            ]
