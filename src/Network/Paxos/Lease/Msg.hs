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

module Network.Paxos.Lease.Msg
    ( Msg(..)
    , isRequest
    , isProposeResponse
    , isPrepareResponse
    , isResponse
    , tests
    ) where

import Control.Monad (liftM2, liftM3, liftM4, liftM5)

import Data.Char (chr, ord)
import qualified Data.Word as W

import Data.Binary (Binary, Get, Put, decode, encode,
    get, getWord8, put, putWord8)

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, Property, arbitraryBoundedIntegral,
    choose, shrinkIntegral, shrinkNothing, (==>))

type NodeId = W.Word16

type ProposalId = W.Word64
type AcceptedProposalId = ProposalId

type LeaseOwner = NodeId

type Duration = W.Word16
type LocalExpireTime = W.Word64

type PaxosId = W.Word64

data Msg = PrepareRequest NodeId ProposalId PaxosId
         | PrepareRejected NodeId ProposalId
         | PreparePreviouslyAccepted NodeId ProposalId AcceptedProposalId
               LeaseOwner Duration
         | PrepareCurrentlyOpen NodeId ProposalId

         | ProposeRequest NodeId ProposalId LeaseOwner Duration
         | ProposeRejected NodeId ProposalId
         | ProposeAccepted NodeId ProposalId

         | LearnChosen NodeId LeaseOwner Duration LocalExpireTime
    deriving (Eq, Show)

putc :: Char -> Put
putc = putWord8 . fromIntegral . ord
{-# INLINE putc #-}

getc :: Get Char
getc = fmap (chr . fromIntegral) getWord8
{-# INLINE getc #-}

instance Binary Msg where
    put (PrepareRequest nodeID proposalID paxosID) = do
        putc '1'
        put nodeID
        put proposalID
        put paxosID

    put (PrepareRejected nodeID proposalID) = do
        putc '2'
        put nodeID
        put proposalID

    put (PreparePreviouslyAccepted nodeID proposalID acceptedProposalID
            leaseOwner duration) = do
        putc '3'
        put nodeID
        put proposalID
        put acceptedProposalID
        put leaseOwner
        put duration

    put (PrepareCurrentlyOpen nodeID proposalID) = do
        putc '4'
        put nodeID
        put proposalID

    put (ProposeRequest nodeID proposalID leaseOwner duration) = do
        putc '5'
        put nodeID
        put proposalID
        put leaseOwner
        put duration

    put (ProposeRejected nodeID proposalID) = do
        putc '6'
        put nodeID
        put proposalID

    put (ProposeAccepted nodeID proposalID) = do
        putc '7'
        put nodeID
        put proposalID

    put (LearnChosen nodeID leaseOwner duration localExpireTime) = do
        putc '8'
        put nodeID
        put leaseOwner
        put duration
        put localExpireTime

    get = do
        tag <- getc
        case tag of
            '1' -> liftM3 PrepareRequest get get get
            '2' -> liftM2 PrepareRejected get get
            '3' -> liftM5 PreparePreviouslyAccepted get get get get get
            '4' -> liftM2 PrepareCurrentlyOpen get get
            '5' -> liftM4 ProposeRequest get get get get
            '6' -> liftM2 ProposeRejected get get
            '7' -> liftM2 ProposeAccepted get get
            '8' -> liftM4 LearnChosen get get get get
            _ -> error "Invalid message tag"


isRequest :: Msg -> Bool
isRequest (PrepareRequest _ _ _) = True
isRequest (ProposeRequest _ _ _ _) = True
isRequest _ = False

isPrepareResponse :: Msg -> Bool
isPrepareResponse (PrepareRejected _ _) = True
isPrepareResponse (PreparePreviouslyAccepted _ _ _ _ _) = True
isPrepareResponse (PrepareCurrentlyOpen _ _) = True
isPrepareResponse _ = False

isProposeResponse :: Msg -> Bool
isProposeResponse (ProposeRejected _ _) = True
isProposeResponse (ProposeAccepted _ _) = True
isProposeResponse _ = False

isResponse :: Msg -> Bool
isResponse m = isPrepareResponse m || isProposeResponse m


tests :: [Test]
tests = [ testGroup "Message serialization"
            [ testProperty "Inverse" prop_serialize
            ]
        , testGroup "Message types"
            [ testProperty "Every message is a request or a response"
                  prop_req_resp
            , testProperty "Every request is a request" prop_req
            , testProperty "Every response is a response" prop_resp
            , testProperty "Every prepare response is a response"
                  prop_resp_prepare
            , testProperty "Every propose response is a response"
                  prop_resp_propose
            , testProperty "Every response is a prepare or propose response"
                  prop_resp_prepare_propose
            ]
        ]

instance Arbitrary Msg where
    arbitrary = do
        n <- choose (1, 8) :: Gen Int
        case n of
            1 -> liftM3 PrepareRequest arbitrary arbitrary arbitrary
            2 -> liftM2 PrepareRejected arbitrary arbitrary
            3 -> liftM5 PreparePreviouslyAccepted arbitrary arbitrary
                     arbitrary arbitrary arbitrary
            4 -> liftM2 PrepareCurrentlyOpen arbitrary arbitrary
            5 -> liftM4 ProposeRequest arbitrary arbitrary arbitrary arbitrary
            6 -> liftM2 ProposeRejected arbitrary arbitrary
            7 -> liftM2 ProposeAccepted arbitrary arbitrary
            8 -> liftM4 LearnChosen arbitrary arbitrary arbitrary arbitrary
            _ -> error "Invalid choice"

    shrink = shrinkNothing

instance Arbitrary W.Word64 where
    arbitrary = arbitraryBoundedIntegral
    shrink = shrinkIntegral

instance Arbitrary W.Word16 where
    arbitrary = arbitraryBoundedIntegral
    shrink = shrinkIntegral

prop_serialize :: Msg -> Bool
prop_serialize msg = decode . encode $ msg == msg

prop_req_resp :: Msg -> Bool
prop_req_resp (LearnChosen _ _ _ _) = True
prop_req_resp msg = isRequest msg /= isResponse msg

prop_req :: Msg -> Bool
prop_req msg@(PrepareRequest _ _ _) = isRequest msg
prop_req msg@(ProposeRequest _ _ _ _) = isRequest msg
prop_req msg = not $ isRequest msg

prop_resp :: Msg -> Bool
prop_resp msg@(PrepareRequest _ _ _) = not $ isResponse msg
prop_resp msg@(ProposeRequest _ _ _ _) = not $ isResponse msg
prop_resp msg@(LearnChosen _ _ _ _) = not $ isResponse msg
prop_resp msg = isResponse msg

prop_resp_prepare :: Msg -> Property
prop_resp_prepare msg = isPrepareResponse msg ==> isResponse msg

prop_resp_propose :: Msg -> Property
prop_resp_propose msg = isProposeResponse msg ==> isResponse msg

prop_resp_prepare_propose :: Msg -> Property
prop_resp_prepare_propose msg = isResponse msg ==>
    isPrepareResponse msg /= isProposeResponse msg
