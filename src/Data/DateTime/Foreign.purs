module Data.DateTime.Foreign where

import Prelude
import Effect.Unsafe (unsafePerformEffect)
import Data.DateTime as D
import Foreign (F, ForeignError(..), fail)
import Foreign.Class (class Encode, class Decode, decode, encode)
import Data.JSDate (fromDateTime, parse, toDate, toDateTime, toISOString, toDateString)
import Data.Maybe (maybe',fromJust)
import Data.Time as T
import Data.Enum
import Partial.Unsafe

newtype DateTime = DateTime D.DateTime

derive newtype instance eqDateTime :: Eq DateTime
derive newtype instance ordDateTime :: Ord DateTime
derive newtype instance boundedDateTime :: Bounded DateTime
derive newtype instance showDateTime :: Show DateTime

invalidDateTime :: String -> Unit -> F DateTime
invalidDateTime datetime _ = fail $ ForeignError $ "Invalid datetime: " <> datetime

instance decodeDateTime :: Decode DateTime where
    decode value = do
        datetimeString <- decode value
        let jsDate = unsafePerformEffect $ parse datetimeString
        maybe' (invalidDateTime datetimeString) (pure <<< DateTime) $ toDateTime jsDate

instance encodeDateTime :: Encode DateTime where
    encode (DateTime dt) = encode $ unsafePerformEffect $ toISOString $ fromDateTime dt

--

newtype Date = Date D.Date

derive newtype instance eqDate :: Eq Date
derive newtype instance ordDate :: Ord Date
derive newtype instance boundedDate :: Bounded Date
derive newtype instance showDate :: Show Date

invalidDate :: String -> Unit -> F Date
invalidDate date _ = fail $ ForeignError $ "Invalid date: " <> date

instance decodeDate :: Decode Date where
    decode value = do
        dateString <- decode value
        let jsDate = unsafePerformEffect $ parse dateString
        maybe' (invalidDate dateString) (pure <<< Date) $ toDate jsDate

instance encodeDate :: Encode Date where
    encode (Date d) = encode $ toDateString $ fromDateTime $ D.DateTime d $ unsafePartial $ fromJust $ T.Time <$> toEnum 0 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0
