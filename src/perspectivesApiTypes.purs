module Perspectives.ApiTypes where

import Prelude

import Foreign (unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object, empty) as F

-- | Identifies Requests with Responses.
type CorrelationIdentifier = String

type ID = String
type Value = String
type ContextID = String

-----------------------------------------------------------
-- REQUEST
-----------------------------------------------------------

-- | The Perspectives Core accepts only a limited set of request types.
data RequestType =
    GetRolBinding
  | GetBinding
  | GetBindingType
  | GetRol
  | GetRolContext
  | GetContextType
  | GetProperty
  | GetViewProperties
  | Unsubscribe
  | ShutDown

derive instance genericRequestType :: Generic RequestType _

instance decodeRequestType :: Decode RequestType where
  decode = genericDecode defaultOptions

instance encodeRequestType :: Encode RequestType where
  encode GetRolBinding = unsafeToForeign "GetRolBinding"
  encode GetBinding = unsafeToForeign "GetBinding"
  encode GetBindingType = unsafeToForeign "GetBindingType"
  encode GetRol = unsafeToForeign "GetRol"
  encode GetRolContext = unsafeToForeign "GetRolContext"
  encode GetContextType = unsafeToForeign "GetContextType"
  encode GetProperty = unsafeToForeign "GetProperty"
  encode GetViewProperties = unsafeToForeign "GetViewProperties"
  encode Unsubscribe = unsafeToForeign "Unsubscribe"
  encode ShutDown = unsafeToForeign "ShutDown"

-- | A request as can be sent to the core.
newtype Request = Request
  { rtype :: RequestType
  , subject :: String
  , predicate :: String
  , object :: String
  , setterId :: CorrelationIdentifier
  , contextDescription :: ContextSerialization
  , rolDescription :: RolSerialization}

derive instance genericRequest :: Generic Request _

requestOptions :: Options
requestOptions = defaultOptions { unwrapSingleConstructors = true }

instance decodeRequest :: Decode Request where
  decode = genericDecode requestOptions

instance encodeRequest :: Encode Request where
  encode = genericEncode requestOptions

-----------------------------------------------------------
-- RESPONSE
-----------------------------------------------------------

-- | The Perspectives Core responds with query results, where a query is the request for the
-- | 'objects' in the basic fact <subject, predicate, object>.
type Object = String

newtype Response = Response {corrId :: CorrelationIdentifier, objects :: Array Object}

derive instance genericResponse :: Generic Response _

instance encodeIdentifiableObjects :: Encode Response where
  encode = genericEncode requestOptions

instance decodeIdentifiableObjects :: Decode Response where
  decode = genericDecode requestOptions

response ::  CorrelationIdentifier -> Array Object -> Response
response corrId objects = Response {corrId, objects}

-----------------------------------------------------------
-- SERIALIZATION OF CONTEXTS AND ROLES ON THE API
-- These types are simpler versions of PerspectContext and PerspectRol.
-- Not meant to put into couchdb, but to use as transport format over the API (whether the TCP or internal channel).
-----------------------------------------------------------
newtype ContextsSerialisation = ContextsSerialisation (Array ContextSerialization)

derive instance genericContextsSerialisation :: Generic ContextsSerialisation _

instance encodeContextsSerialisation :: Encode ContextsSerialisation where
  encode = genericEncode requestOptions

instance decodeContextsSerialisation :: Decode ContextsSerialisation where
  decode = genericDecode requestOptions

newtype ContextSerialization = ContextSerialization ContextSerializationRecord

type ContextSerializationRecord =
  { id :: String
  , prototype :: Maybe ContextID
  , ctype :: ContextID
  , rollen :: F.Object (Array RolSerialization)
  , interneProperties :: PropertySerialization
  , externeProperties :: PropertySerialization
}

newtype RolSerialization = RolSerialization
  { properties :: PropertySerialization
  , binding :: ID
}
defaultContextSerializationRecord :: ContextSerializationRecord
defaultContextSerializationRecord = {id: "", prototype: Nothing, ctype: "", rollen: F.empty, interneProperties: PropertySerialization F.empty, externeProperties: PropertySerialization F.empty}

newtype PropertySerialization = PropertySerialization (F.Object (Array Value))

derive instance genericContextSerialization :: Generic ContextSerialization _

instance encodeContextSerialization :: Encode ContextSerialization where
  encode = genericEncode requestOptions

instance decodeContextSerialization :: Decode ContextSerialization where
  decode = genericDecode requestOptions

derive instance genericRolSerialization :: Generic RolSerialization _

instance encodeRolSerialization :: Encode RolSerialization where
  encode = genericEncode requestOptions

instance decodeRolSerialization :: Decode RolSerialization where
  decode = genericDecode requestOptions

derive instance genericPropertySerialization :: Generic PropertySerialization _

instance encodePropertySerialization :: Encode PropertySerialization where
  encode = genericEncode requestOptions

instance decodePropertySerialization :: Decode PropertySerialization where
  decode = genericDecode requestOptions

instance showPropertySerialization :: Show PropertySerialization where
  show (PropertySerialization s) =  show s

instance showRolSerialization :: Show RolSerialization where
  show (RolSerialization {properties, binding}) = "{ " <> show properties <> ", " <> binding <> " }"

instance showContextSerialization :: Show ContextSerialization where
  show (ContextSerialization {id, ctype, rollen, interneProperties, externeProperties}) =
    "{ id=" <> id <> ", ctype=" <> ctype <> ", rollen=" <> show rollen <> ", interneProperties=" <> show interneProperties <> ", externeProperties=" <> show externeProperties
