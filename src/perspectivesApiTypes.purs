module Perspectives.ApiTypes where

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)

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

-- | A request as can be sent to the core.
newtype Request = Request
  { type :: RequestType
  , subject :: String
  , predicate :: String
  , setterId :: CorrelationIdentifier}

derive instance genericRequest :: Generic Request _

requestOptions :: Options
requestOptions = defaultOptions { unwrapSingleConstructors = true }

instance decodeRequest :: Decode Request where
  decode = genericDecode requestOptions

-----------------------------------------------------------
-- RESPONSE
-----------------------------------------------------------

-- | The Perspectives Core responds with query results, where a query is the request for the
-- | 'objects' in the basic fact <subject, predicate, object>.
type Object = String

newtype Response = Response {setterId :: CorrelationIdentifier, objects :: Array Object}

derive instance genericIdentifiableObjects :: Generic Response _

instance encodeIdentifiableObjects :: Encode Response where
  encode = genericEncode requestOptions

response ::  CorrelationIdentifier -> Array Object -> Response
response setterId objects = Response {setterId, objects}
