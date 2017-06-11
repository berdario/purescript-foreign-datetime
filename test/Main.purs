module Test.Main where

import Prelude

import Data.DateTime as D
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except (runExcept)
import Data.Enum (fromEnum, toEnum, class BoundedEnum)
import Data.Foreign.Class (encode, decode)
import Jack (property, check', Property, forAll, Gen, chooseInt, justOf, suchThat)
import Test.Assert (ASSERT, assert)

import Data.DateTime.Foreign (DateTime(..))

main :: forall e. Eff (random :: RANDOM, assert :: ASSERT, console :: CONSOLE | e) Unit
main = do
  log "Check that the Is/AsForeign instances roundtrip"
  assert =<< check' 10000 roundTripsViaJson

boundedEnum :: forall a. BoundedEnum a => Gen a
boundedEnum = justOf $ map toEnum $
                chooseInt (fromEnum (bottom :: a))
                          (fromEnum (top :: a))


dateTime :: Gen DateTime
dateTime = map DateTime (D.DateTime <$> (D.canonicalDate <$> (suchThat boundedEnum notBetween0and100) <*> boundedEnum <*> boundedEnum)
                                    <*> (D.Time <$> boundedEnum <*> boundedEnum <*> boundedEnum <*> boundedEnum))
  where
    -- temporary hack while waiting for https://github.com/purescript-contrib/purescript-js-date/pull/11
    notBetween0and100 = (\x -> x < 0 || x > 100) <<< fromEnum

roundTripsViaJson :: Property
roundTripsViaJson =
  forAll dateTime \dt ->
    property $ pure dt == (runExcept $ decode $ encode dt)

