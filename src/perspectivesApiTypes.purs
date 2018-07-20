module Perspectives.ApiTypes where

import Data.Foreign (toForeign)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.StrMap (StrMap)
import Perspectives.EntiteitAndRDFAliases (ID, Value)

-- | Identifies Requests with Responses.
type CorrelationIdentifier = String

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
  encode GetRolBinding = toForeign "GetRolBinding"
  encode GetBinding = toForeign "GetBinding"
  encode GetBindingType = toForeign "GetBindingType"
  encode GetRol = toForeign "GetRol"
  encode GetRolContext = toForeign "GetRolContext"
  encode GetContextType = toForeign "GetContextType"
  encode GetProperty = toForeign "GetProperty"
  encode GetViewProperties = toForeign "GetViewProperties"
  encode Unsubscribe = toForeign "Unsubscribe"
  encode ShutDown = toForeign "ShutDown"

-- | A request as can be sent to the core.
newtype Request = Request
  { rtype :: RequestType
  , subject :: String
  , predicate :: String
  , setterId :: CorrelationIdentifier}

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
newtype ContextSerialization = ContextSerialization
  { context :: String
  , rollen :: StrMap RolSerialization
  , interneProperties :: PropertySerialization
  , externeProperties :: PropertySerialization
}

newtype RolSerialization = RolSerialization
  { properties :: PropertySerialization
  , binding :: ID
}

newtype PropertySerialization = PropertySerialization (StrMap (Array Value))

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
