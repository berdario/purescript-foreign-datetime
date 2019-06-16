module Test.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.DateTime as D
import Data.Enum (fromEnum, toEnum, class BoundedEnum)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Class (encode, decode)
import Jack (property, check', Property, forAll, Gen, chooseInt, justOf)
import Test.Assert (assert)

import Data.DateTime.Foreign (DateTime(..))

main :: Effect Unit
main = do
  log "Check that the Is/AsForeign instances roundtrip"
  assert =<< check' 10000 roundTripsViaJson

boundedEnum :: forall a. BoundedEnum a => Gen a
boundedEnum = justOf $ map toEnum $
                chooseInt (fromEnum (bottom :: a))
                          (fromEnum (top :: a))


dateTime :: Gen DateTime
dateTime = map DateTime (D.DateTime <$> (D.canonicalDate <$> boundedEnum <*> boundedEnum <*> boundedEnum)
                                    <*> (D.Time <$> boundedEnum <*> boundedEnum <*> boundedEnum <*> boundedEnum))

roundTripsViaJson :: Property
roundTripsViaJson =
  forAll dateTime \dt ->
    property $ pure dt == (runExcept $ decode $ encode dt)

