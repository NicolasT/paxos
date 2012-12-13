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

module Network.Paxos.Synod (
    -- * Re-exports
      BroadcastGroup(..)
    , Action(..)
    , Message(..)
    , Quorum
    , quorum
    -- * Testing
    , tests
    ) where

import Test.Framework (Test, testGroup)

import Network.Paxos.Synod.Action
import Network.Paxos.Synod.Messages hiding (tests)
import Network.Paxos.Synod.Types hiding (tests)

import qualified Network.Paxos.Synod.Types
import qualified Network.Paxos.Synod.Proposer
import qualified Network.Paxos.Synod.Acceptor
import qualified Network.Paxos.Synod.Learner
import qualified Network.Paxos.Synod.Messages

-- | Tests for modules in "Network.Paxos.Synod"
tests :: Test
tests = testGroup "Network.Paxos.Synod" [
          Network.Paxos.Synod.Types.tests
        , Network.Paxos.Synod.Proposer.tests
        , Network.Paxos.Synod.Acceptor.tests
        , Network.Paxos.Synod.Learner.tests
        , Network.Paxos.Synod.Messages.tests
        ]
