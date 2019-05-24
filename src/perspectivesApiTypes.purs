module Perspectives.ApiTypes where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Types (Options)
import Foreign.Object (Object, empty) as F
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- | Identifies Requests with Responses.
type CorrelationIdentifier = Int

type ID = String
type Value = String
type ContextID = String

-----------------------------------------------------------
-- APIEFFECT
-----------------------------------------------------------
-- | The type of functions that are passed on as callbacks through the API.
type ApiEffect = ResponseRecord -> Effect Unit

mkApiEffect :: Maybe Foreign -> ApiEffect
mkApiEffect f = unsafeCoerce $ unsafePartial $ fromJust f
-----------------------------------------------------------
-- REQUEST
-----------------------------------------------------------

-- | The Perspectives Core accepts only a limited set of request types.
data RequestType =
    GetRolBinding
  | GetBinding
  | GetBindingType
  | GetRolContext
  | GetContextType
  | GetRolType
  | GetRol
  | GetProperty
  | GetViewProperties
  | ShutDown
  | Unsubscribe
  | CreateContext
  | DeleteContext
  | CreateRol
  | AddRol
  | SetBinding
  | SetProperty
  | WrongRequest

derive instance genericRequestType :: Generic RequestType _

instance decodeRequestType :: Decode RequestType where
  -- decode = genericDecode defaultOptions
  decode s = except $ Right $ case unsafeCoerce s of
    "GetRolBinding" -> GetRolBinding
    "GetBinding" -> GetBinding
    "GetBindingType" -> GetBindingType
    "GetRol" -> GetRol
    "GetRolContext" -> GetRolContext
    "GetContextType" -> GetContextType
    "GetProperty" -> GetProperty
    "GetViewProperties" -> GetViewProperties
    "Unsubscribe" -> Unsubscribe
    "ShutDown" -> ShutDown
    "GetRolType" -> GetRolType
    "CreateContext" -> CreateContext
    "DeleteContext" -> DeleteContext
    "CreateRol" -> CreateRol
    "AddRol" -> AddRol
    "SetBinding" -> SetBinding
    "SetProperty" -> SetProperty
    _ -> WrongRequest

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
  encode GetRolType = unsafeToForeign "GetRolType"
  encode CreateContext = unsafeToForeign "CreateContext"
  encode DeleteContext = unsafeToForeign "DeleteContext"
  encode CreateRol = unsafeToForeign "CreateRol"
  encode AddRol = unsafeToForeign "AddRol"
  encode SetBinding = unsafeToForeign "SetBinding"
  encode SetProperty = unsafeToForeign "SetProperty"
  encode WrongRequest = unsafeToForeign "WrongRequest"

instance showRequestType :: Show RequestType where
  show GetRolBinding = "GetRolBinding"
  show GetBinding = "GetBinding"
  show GetBindingType = "GetBindingType"
  show GetRol = "GetRol"
  show GetRolContext = "GetRolContext"
  show GetContextType = "GetContextType"
  show GetProperty = "GetProperty"
  show GetViewProperties = "GetViewProperties"
  show Unsubscribe = "Unsubscribe"
  show ShutDown = "ShutDown"
  show GetRolType = "GetRolType"
  show CreateContext = "CreateContext"
  show DeleteContext = "DeleteContext"
  show CreateRol = "CreateRol"
  show AddRol = "AddRol"
  show SetBinding = "SetBinding"
  show SetProperty = "SetProperty"
  show WrongRequest = "WrongRequest"

instance eqRequestType :: Eq RequestType where
  eq r1 r2 = show r1 == show r2

-- | A request as can be sent to the core.
newtype Request = Request RequestRecord

type RequestRecord =
  { request :: RequestType
  , subject :: String
  , predicate :: String
  , object :: String
  , reactStateSetter :: Maybe Foreign
  , corrId :: CorrelationIdentifier
  , contextDescription :: Foreign
  , rolDescription :: Maybe RolSerialization}

derive instance genericRequest :: Generic Request _

derive instance newTypeRequest :: Newtype Request _

showRequestRecord :: RequestRecord -> String
showRequestRecord {request, subject, predicate} = "{" <> show request <> ", " <> subject <> ", " <> predicate <> "}"

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

newtype ResponseRecord = ResponseRecord {corrId :: CorrelationIdentifier, result :: Maybe (Array Object), error :: Maybe String}

derive instance genericResponse :: Generic ResponseRecord _

instance encodeResponseRecord :: Encode ResponseRecord where
  -- encode = genericEncode requestOptions
  encode (ResponseRecord{corrId, result, error}) = case result of
    Nothing -> unsafeToForeign {corrId: corrId, error: unsafePartial $ fromJust error}
    (Just r) -> unsafeToForeign {corrId: corrId, result: r}

data Response = Result CorrelationIdentifier (Array String) | Error CorrelationIdentifier String

-- response ::  CorrelationIdentifier -> Array Object -> Response
-- response corrId objects = Response {corrId, objects}

convertResponse :: Response -> Foreign
convertResponse (Result i s) = unsafeToForeign {corrId: i, result: s}
convertResponse (Error i s) = unsafeToForeign {corrId: i, error: s}

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
  , binding :: Maybe ID
}
defaultContextSerializationRecord :: ContextSerializationRecord
defaultContextSerializationRecord = {id: "", prototype: Nothing, ctype: "", rollen: F.empty, interneProperties: PropertySerialization F.empty, externeProperties: PropertySerialization F.empty}

newtype PropertySerialization = PropertySerialization (F.Object (Array Value))

derive instance genericContextSerialization :: Generic ContextSerialization _

instance encodeContextSerialization :: Encode ContextSerialization where
  encode = genericEncode requestOptions

instance decodeContextSerialization :: Decode ContextSerialization where
  decode = genericDecode requestOptions
  -- decode = unsafeFromForeign >>> readJSON'

-- instance readForeignContextSerialisation :: ReadForeign ContextSerialization where
--   readImpl x = readJSON' (unsafeFromForeign x)

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
  show (RolSerialization {properties, binding}) = "{ " <> show properties <> ", " <> show binding <> " }"

instance showContextSerialization :: Show ContextSerialization where
  show (ContextSerialization {id, ctype, rollen, interneProperties, externeProperties}) =
    "{ id=" <> id <> ", ctype=" <> ctype <> ", rollen=" <> show rollen <> ", interneProperties=" <> show interneProperties <> ", externeProperties=" <> show externeProperties
