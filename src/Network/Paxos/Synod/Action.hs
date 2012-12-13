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

module Network.Paxos.Synod.Action (
      BroadcastGroup(..)
    , Action(..)
    ) where

import Network.Paxos.Synod.Messages

-- | Broadcast group identifier
data BroadcastGroup = Acceptors
                    | Learners
  deriving (Show, Eq)


-- | Actions which might need to be executed as the result of a state
-- transition
data Action nodeId value = Send nodeId (Message nodeId value)
                         -- ^ Send the given message to the given node
                         | Broadcast BroadcastGroup (Message nodeId value)
                         -- ^ Broadcast the given message to the given group
  deriving (Show, Eq)

