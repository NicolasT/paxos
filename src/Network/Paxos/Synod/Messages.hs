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

module Network.Paxos.Synod.Messages (
      Prepare(Prepare)
    , Promise(Promise)
    , Accept(Accept)
    , Accepted(Accepted)
    , Message(..)
    ) where

import Control.Applicative

import Test.QuickCheck (Arbitrary, arbitrary)

import Network.Paxos.Synod.Types

data Prepare nodeId = Prepare (ProposalId nodeId)
  deriving (Show, Eq)

instance Arbitrary nodeId => Arbitrary (Prepare nodeId) where
    arbitrary = Prepare <$> arbitrary


data Promise nodeId value = Promise (ProposalId nodeId) (Maybe (AcceptedValue nodeId value))
  deriving (Show, Eq)

instance (Arbitrary nodeId, Arbitrary value) => Arbitrary (Promise nodeId value) where
    arbitrary = Promise <$> arbitrary <*> arbitrary


data Accept nodeId value = Accept (ProposalId nodeId) value
  deriving (Show, Eq)

instance (Arbitrary nodeId, Arbitrary value) => Arbitrary (Accept nodeId value) where
    arbitrary = Accept <$> arbitrary <*> arbitrary


data Accepted nodeId value = Accepted (ProposalId nodeId) value
  deriving (Show, Eq)

instance (Arbitrary nodeId, Arbitrary value) => Arbitrary (Accepted nodeId value) where
    arbitrary = Accepted <$> arbitrary <*> arbitrary


-- | Union type for all types of message which might flow across nodes
data Message nodeId value = MsgPrepare (Prepare nodeId)
                          -- ^ A `Prepare' message, from Proposer to Acceptor
                          | MsgPromise (Promise nodeId value)
                          -- ^ A `Promise' message, from Acceptor to Proposer
                          | MsgAccept (Accept nodeId value)
                          -- ^ An `Accept' message, from Proposer to Acceptor
                          | MsgAccepted (Accepted nodeId value)
                          -- ^ An `Accepted' message, from Acceptor to Learner
  deriving (Show, Eq)
