module Data.AddressBook where

import Prelude
import Control.MonadZero (empty)
import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)
import Data.Monoid ((<>))
import Data.Semiring (add)

add :: Int -> Int -> Int
add x y = x + y

type Entry = 
    { firstName :: String
    , lastName :: String
    , address :: Address }

type Address = 
    { street:: String
    , city:: String 
    , state:: String }

type AddressBook = List Entry

showEntry ::  Entry -> String
showEntry entry = 
    entry.lastName  <> ", " <>
    entry.firstName <> ", " <>
    showAddress entry.address

showAddress :: Address -> String
showAddress address = 
    address.street  <> ", " <>
    address.city    <> ", " <>
    address.state


emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons 

-- filter:: (Entry -> Boolean) -> AddressBook -> AddressBook
-- head:: AddressBook -> Maybe Entry

findEntry firstName lastName book = head $ filter filterEntry book
    where
      filterEntry :: Entry -> Boolean
      filterEntry entry = entry.firstName == firstName && entry.lastName == lastName