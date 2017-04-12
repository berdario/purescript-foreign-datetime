module Data.DateTime.Foreign where

import Prelude
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.DateTime as D
import Data.Foreign (F, ForeignError(..), fail)
import Data.Foreign.Class (class AsForeign, class IsForeign, read, write)
import Data.Generic (class Generic)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString)
import Data.Maybe (maybe')

newtype DateTime = DateTime D.DateTime

derive newtype instance eqDateTime :: Eq DateTime
derive newtype instance ordDateTime :: Ord DateTime
derive newtype instance genericDateTime :: Generic DateTime
derive newtype instance boundedDateTime :: Bounded DateTime
derive newtype instance showDateTime :: Show DateTime

invalidDate :: String -> Unit -> F DateTime
invalidDate date _ = fail $ ForeignError $ "Invalid date: " <> date

instance isForeignDateTime :: IsForeign DateTime where
    read value = do
        dateString <- read value
        let jsDate = unsafePerformEff $ parse dateString
        maybe' (invalidDate dateString) (pure <<< DateTime) $ toDateTime jsDate

instance asForeignDateTime :: AsForeign DateTime where
    write (DateTime dt) = write $ unsafePerformEff $ toISOString $ fromDateTime dt
