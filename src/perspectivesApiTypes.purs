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
type ApiEffect = Response -> Effect Unit

mkApiEffect :: Maybe Foreign -> ApiEffect
mkApiEffect f = (unsafeCoerce $ unsafePartial $ fromJust f) <<< convertResponse
-----------------------------------------------------------
-- REQUEST
-----------------------------------------------------------

-- | The Perspectives Core accepts only a limited set of request types.
data RequestType =
  -- Consulting
    GetRolBinding
  | GetBinding
  | GetBindingType
  | GetRolContext
  | GetContextType
  | GetRolType
  | GetUnqualifiedRolType
  | GetRol
  | GetUnqualifiedRol
  | GetProperty
  | GetViewProperties
  | GetMeForContext

  -- Pure Deltas
  | CreateContext
  | DeleteContext
  | CreateRol
  | RemoveRol
  | AddRol
  | SetBinding
  | RemoveBinding
  | SetProperty

  -- Conveniences
  | CreateRolWithLocalName
  | BindInNewRol

  -- Meta
  | Unsubscribe
  | ShutDown
  | CheckBinding
  | WrongRequest

derive instance genericRequestType :: Generic RequestType _

instance decodeRequestType :: Decode RequestType where
  -- decode = genericDecode defaultOptions
  decode s = except $ Right $ case unsafeCoerce s of
    "GetRolBinding" -> GetRolBinding
    "GetBinding" -> GetBinding
    "GetBindingType" -> GetBindingType
    "GetRol" -> GetRol
    "GetUnqualifiedRol" -> GetUnqualifiedRol
    "GetRolContext" -> GetRolContext
    "GetContextType" -> GetContextType
    "GetProperty" -> GetProperty
    "GetViewProperties" -> GetViewProperties
    "GetMeForContext" -> GetMeForContext
    "Unsubscribe" -> Unsubscribe
    "ShutDown" -> ShutDown
    "GetRolType" -> GetRolType
    "GetUnqualifiedRolType" -> GetUnqualifiedRolType
    "CreateContext" -> CreateContext
    "DeleteContext" -> DeleteContext
    "CreateRol" -> CreateRol
    "CreateRolWithLocalName" -> CreateRolWithLocalName
    "AddRol" -> AddRol
    "RemoveRol" -> RemoveRol
    "SetBinding" -> SetBinding
    "RemoveBinding" -> RemoveBinding
    "BindInNewRol" -> BindInNewRol
    "CheckBinding" -> CheckBinding
    "SetProperty" -> SetProperty
    _ -> WrongRequest

instance encodeRequestType :: Encode RequestType where
  encode GetRolBinding = unsafeToForeign "GetRolBinding"
  encode GetBinding = unsafeToForeign "GetBinding"
  encode GetBindingType = unsafeToForeign "GetBindingType"
  encode GetRol = unsafeToForeign "GetRol"
  encode GetUnqualifiedRol = unsafeToForeign "GetUnqualifiedRol"
  encode GetRolContext = unsafeToForeign "GetRolContext"
  encode GetContextType = unsafeToForeign "GetContextType"
  encode GetProperty = unsafeToForeign "GetProperty"
  encode GetViewProperties = unsafeToForeign "GetViewProperties"
  encode GetMeForContext = unsafeToForeign "GetMeForContext"
  encode Unsubscribe = unsafeToForeign "Unsubscribe"
  encode ShutDown = unsafeToForeign "ShutDown"
  encode GetRolType = unsafeToForeign "GetRolType"
  encode GetUnqualifiedRolType = unsafeToForeign "GetUnqualifiedRolType"
  encode CreateContext = unsafeToForeign "CreateContext"
  encode DeleteContext = unsafeToForeign "DeleteContext"
  encode CreateRol = unsafeToForeign "CreateRol"
  encode CreateRolWithLocalName = unsafeToForeign "CreateRolWithLocalName"
  encode AddRol = unsafeToForeign "AddRol"
  encode RemoveRol = unsafeToForeign "RemoveRol"
  encode SetBinding = unsafeToForeign "SetBinding"
  encode RemoveBinding = unsafeToForeign "RemoveBinding"
  encode BindInNewRol = unsafeToForeign "BindInNewRol"
  encode CheckBinding = unsafeToForeign "CheckBinding"
  encode SetProperty = unsafeToForeign "SetProperty"
  encode WrongRequest = unsafeToForeign "WrongRequest"

instance showRequestType :: Show RequestType where
  show GetRolBinding = "GetRolBinding"
  show GetBinding = "GetBinding"
  show GetBindingType = "GetBindingType"
  show GetRol = "GetRol"
  show GetUnqualifiedRol = "GetUnqualifiedRol"
  show GetRolContext = "GetRolContext"
  show GetContextType = "GetContextType"
  show GetProperty = "GetProperty"
  show GetViewProperties = "GetViewProperties"
  show GetMeForContext = "GetMeForContext"
  show Unsubscribe = "Unsubscribe"
  show ShutDown = "ShutDown"
  show GetRolType = "GetRolType"
  show GetUnqualifiedRolType = "GetUnqualifiedRolType"
  show CreateContext = "CreateContext"
  show DeleteContext = "DeleteContext"
  show CreateRol = "CreateRol"
  show CreateRolWithLocalName = "CreateRolWithLocalName"
  show AddRol = "AddRol"
  show RemoveRol = "RemoveRol"
  show SetBinding = "SetBinding"
  show RemoveBinding = "RemoveBinding"
  show BindInNewRol = "BindInNewRol"
  show CheckBinding = "CheckBinding"
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

data Response = Result CorrelationIdentifier (Array String) | Error CorrelationIdentifier String

instance encodeReponse :: Encode Response where
  encode = convertResponse

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
  , externeProperties :: PropertySerialization
}

newtype RolSerialization = RolSerialization
  { properties :: PropertySerialization
  , binding :: Maybe ID
}
defaultContextSerializationRecord :: ContextSerializationRecord
defaultContextSerializationRecord = {id: "", prototype: Nothing, ctype: "", rollen: F.empty, externeProperties: PropertySerialization F.empty}

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
  show (ContextSerialization {id, ctype, rollen, externeProperties}) =
    "{ id=" <> id <> ", ctype=" <> ctype <> ", rollen=" <> show rollen <> ", externeProperties=" <> show externeProperties
