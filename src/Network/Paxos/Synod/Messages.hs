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

module Network.Paxos.Synod.Messages (
      Prepare(Prepare)
    , Promise(Promise)
    , Accept(Accept)
    , Accepted(Accepted)
    , Message(..)
    , tests
    ) where

import Control.Applicative

import Data.Serialize
import Data.Typeable (Typeable)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck (Arbitrary, arbitrary, oneof)

import Network.Paxos.Synod.Types hiding (tests)

data Prepare nodeId = Prepare (ProposalId nodeId)
  deriving (Show, Eq, Typeable)

instance Serialize nodeId => Serialize (Prepare nodeId) where
    get = Prepare <$> get
    put (Prepare nodeId) = put nodeId

instance Arbitrary nodeId => Arbitrary (Prepare nodeId) where
    arbitrary = Prepare <$> arbitrary


data Promise nodeId value = Promise (ProposalId nodeId) (Maybe (AcceptedValue nodeId value))
  deriving (Show, Eq, Typeable)

instance (Serialize nodeId, Serialize value) => Serialize (Promise nodeId value) where
    get = Promise <$> get <*> get
    put (Promise p m) = put p >> put m

instance (Arbitrary nodeId, Arbitrary value) => Arbitrary (Promise nodeId value) where
    arbitrary = Promise <$> arbitrary <*> arbitrary


data Accept nodeId value = Accept (ProposalId nodeId) value
  deriving (Show, Eq, Typeable)

instance (Serialize nodeId, Serialize value) => Serialize (Accept nodeId value) where
    get = Accept <$> get <*> get
    put (Accept p v) = put p >> put v

instance (Arbitrary nodeId, Arbitrary value) => Arbitrary (Accept nodeId value) where
    arbitrary = Accept <$> arbitrary <*> arbitrary


data Accepted nodeId value = Accepted (ProposalId nodeId) value
  deriving (Show, Eq, Typeable)

instance (Serialize nodeId, Serialize value) => Serialize (Accepted nodeId value) where
    get = Accepted <$> get <*> get
    put (Accepted p v) = put p >> put v

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
                          | MsgUnknown
                          -- ^ Some unknown message was received, and (e.g.) parsing failed
  deriving (Show, Eq, Typeable)

instance (Serialize nodeId, Serialize value) => Serialize (Message nodeId value) where
    get = do
        tag <- getWord8
        case tag of
            1 -> MsgPrepare <$> get
            2 -> MsgPromise <$> get
            3 -> MsgAccept <$> get
            4 -> MsgAccepted <$> get
            _ -> return MsgUnknown

    put msg = case msg of
        MsgPrepare m -> putWord8 1 >> put m
        MsgPromise m -> putWord8 2 >> put m
        MsgAccept m -> putWord8 3 >> put m
        MsgAccepted m -> putWord8 4 >> put m
        MsgUnknown -> error "put: can't serialize MsgUnknown"

instance (Arbitrary nodeId, Arbitrary value) => Arbitrary (Message nodeId value) where
    arbitrary = oneof [ MsgPrepare <$> arbitrary
                      , MsgPromise <$> arbitrary
                      , MsgAccept <$> arbitrary
                      , MsgAccepted <$> arbitrary
                      ]

prop_serialization :: Message String Int -> Bool
prop_serialization msg = decode (encode msg) == Right msg


tests :: Test
tests = testGroup "Network.Paxos.Synod.Messages" [
              testProperty "serialization" prop_serialization
            ]
