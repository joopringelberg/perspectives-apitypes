module Perspectives.ApiTypes where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Types (Options)
import Foreign.Object (Object, empty) as F
import Partial.Unsafe (unsafePartial)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Simple.JSON (class ReadForeign, class WriteForeign, read', write)
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
    GetBinding
  | GetRoleBinders
  | GetRolContext
  | GetContextType
  | GetRolType
  | GetRoleKind
  | GetUnqualifiedRolType
  | GetRol
  | GetUnqualifiedRol
  | GetProperty
  | GetPropertyFromLocalName
  | GetViewProperties
  | GetMeForContext
  | GetUserIdentifier
  | GetLocalRoleSpecialisation
  | MatchContextName

  -- Pure Deltas
  | CreateContext
  | CreateContext_
  | CreateRol
  | RemoveRol
  | DeleteRole
  | Bind_ -- Formerly SetBinding
  | RemoveBinding
  | SetProperty
  | DeleteProperty

  | ImportContexts
  | ImportTransaction

  -- Conveniences
  | Bind -- Formerly BindInNewRol

  -- Meta
  | Unsubscribe
  | ShutDown
  | CheckBinding
  | WrongRequest

derive instance genericRequestType :: Generic RequestType _

instance decodeRequestType :: Decode RequestType where
  -- decode = genericDecode defaultOptions
  decode s = except $ Right $ case unsafeCoerce s of
    "GetBinding" -> GetBinding
    "GetRoleBinders" -> GetRoleBinders
    "GetRol" -> GetRol
    "GetUnqualifiedRol" -> GetUnqualifiedRol
    "GetRolContext" -> GetRolContext
    "GetContextType" -> GetContextType
    "GetProperty" -> GetProperty
    "GetPropertyFromLocalName" -> GetPropertyFromLocalName
    "GetViewProperties" -> GetViewProperties
    "GetMeForContext" -> GetMeForContext
    "GetUserIdentifier" -> GetUserIdentifier
    "GetLocalRoleSpecialisation" -> GetLocalRoleSpecialisation
    "MatchContextName" -> MatchContextName
    "Unsubscribe" -> Unsubscribe
    "ShutDown" -> ShutDown
    "GetRolType" -> GetRolType
    "GetRoleKind" -> GetRoleKind
    "GetUnqualifiedRolType" -> GetUnqualifiedRolType
    "CreateContext" -> CreateContext
    "CreateContext_" -> CreateContext_
    "CreateRol" -> CreateRol
    "RemoveRol" -> RemoveRol
    "DeleteRole" -> DeleteRole
    "Bind_" -> Bind_
    "RemoveBinding" -> RemoveBinding
    "Bind" -> Bind
    "CheckBinding" -> CheckBinding
    "SetProperty" -> SetProperty
    "DeleteProperty" -> DeleteProperty
    "ImportContexts" -> ImportContexts
    "ImportTransaction" -> ImportTransaction
    _ -> WrongRequest

instance encodeRequestType :: Encode RequestType where
  encode GetBinding = unsafeToForeign "GetBinding"
  encode GetRoleBinders = unsafeToForeign "GetRoleBinders"
  encode GetRol = unsafeToForeign "GetRol"
  encode GetUnqualifiedRol = unsafeToForeign "GetUnqualifiedRol"
  encode GetRolContext = unsafeToForeign "GetRolContext"
  encode GetContextType = unsafeToForeign "GetContextType"
  encode GetProperty = unsafeToForeign "GetProperty"
  encode GetPropertyFromLocalName = unsafeToForeign "GetPropertyFromLocalName"
  encode GetViewProperties = unsafeToForeign "GetViewProperties"
  encode GetMeForContext = unsafeToForeign "GetMeForContext"
  encode GetUserIdentifier = unsafeToForeign "GetUserIdentifier"
  encode GetLocalRoleSpecialisation = unsafeToForeign "GetLocalRoleSpecialisation"
  encode MatchContextName = unsafeToForeign "MatchContextName"
  encode Unsubscribe = unsafeToForeign "Unsubscribe"
  encode ShutDown = unsafeToForeign "ShutDown"
  encode GetRolType = unsafeToForeign "GetRolType"
  encode GetRoleKind = unsafeToForeign "GetRoleKind"
  encode GetUnqualifiedRolType = unsafeToForeign "GetUnqualifiedRolType"
  encode CreateContext = unsafeToForeign "CreateContext"
  encode CreateContext_ = unsafeToForeign "CreateContext_"
  encode CreateRol = unsafeToForeign "CreateRol"
  encode RemoveRol = unsafeToForeign "RemoveRol"
  encode DeleteRole = unsafeToForeign "DeleteRole"
  encode Bind_ = unsafeToForeign "Bind_"
  encode RemoveBinding = unsafeToForeign "RemoveBinding"
  encode Bind = unsafeToForeign "Bind"
  encode CheckBinding = unsafeToForeign "CheckBinding"
  encode SetProperty = unsafeToForeign "SetProperty"
  encode DeleteProperty = unsafeToForeign "DeleteProperty"
  encode ImportContexts = unsafeToForeign "ImportContexts"
  encode ImportTransaction = unsafeToForeign "ImportTransaction"
  encode WrongRequest = unsafeToForeign "WrongRequest"

instance showRequestType :: Show RequestType where
  show GetBinding = "GetBinding"
  show GetRoleBinders = "GetRoleBinders"
  show GetRol = "GetRol"
  show GetUnqualifiedRol = "GetUnqualifiedRol"
  show GetRolContext = "GetRolContext"
  show GetContextType = "GetContextType"
  show GetProperty = "GetProperty"
  show GetPropertyFromLocalName = "GetPropertyFromLocalName"
  show GetViewProperties = "GetViewProperties"
  show GetMeForContext = "GetMeForContext"
  show GetUserIdentifier = "GetUserIdentifier"
  show GetLocalRoleSpecialisation = "GetLocalRoleSpecialisation"
  show MatchContextName = "MatchContextName"
  show Unsubscribe = "Unsubscribe"
  show ShutDown = "ShutDown"
  show GetRolType = "GetRolType"
  show GetRoleKind = "GetRoleKind"
  show GetUnqualifiedRolType = "GetUnqualifiedRolType"
  show CreateContext = "CreateContext"
  show CreateContext_ = "CreateContext_"
  show CreateRol = "CreateRol"
  show RemoveRol = "RemoveRol"
  show DeleteRole = "DeleteRole"
  show Bind_ = "Bind_"
  show RemoveBinding = "RemoveBinding"
  show Bind = "Bind"
  show CheckBinding = "CheckBinding"
  show SetProperty = "SetProperty"
  show DeleteProperty = "DeleteProperty"
  show ImportContexts = "ImportContexts"
  show ImportTransaction = "ImportTransaction"
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
  , rolDescription :: Maybe RolSerialization
  , authoringRole :: Maybe String}

derive instance genericRequest :: Generic Request _

derive instance newTypeRequest :: Newtype Request _

showRequestRecord :: RequestRecord -> String
showRequestRecord {request, subject, predicate} = "{" <> show request <> ", " <> subject <> ", " <> predicate <> "}"

instance showRequest :: Show Request where
  show (Request r) = showRequestRecord r

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

-----------------------------------------------------------
---- CONTEXTSSERIALIZATION
-----------------------------------------------------------
newtype ContextsSerialisation = ContextsSerialisation (Array ContextSerialization)

derive instance genericContextsSerialisation :: Generic ContextsSerialisation _

instance encodeContextsSerialisation :: Encode ContextsSerialisation where
  encode (ContextsSerialisation cs) = encode cs

instance decodeContextsSerialisation :: Decode ContextsSerialisation where
  decode = decode >=> pure <<< ContextsSerialisation

-----------------------------------------------------------
---- CONTEXTSERIALIZATION
-----------------------------------------------------------
newtype ContextSerialization = ContextSerialization ContextSerializationRecord

type ContextSerializationRecord =
  { id :: String
  , prototype :: Maybe ContextID
  , ctype :: ContextID
  , rollen :: F.Object (SerializableNonEmptyArray RolSerialization)
  , externeProperties :: PropertySerialization
}

derive newtype instance eqContextSerialization :: Eq ContextSerialization

derive instance genericContextSerialization :: Generic ContextSerialization _

instance showContextSerialization :: Show ContextSerialization where
  show (ContextSerialization {id, ctype, rollen, externeProperties}) =
    "{ id=" <> id <> ", ctype=" <> ctype <> ", rollen=" <> show rollen <> ", externeProperties=" <> show externeProperties

instance encodeContextSerialization :: Encode ContextSerialization where
  encode (ContextSerialization c)= write c

instance decodeContextSerialization :: Decode ContextSerialization where
  decode = read' >=> pure <<< ContextSerialization

defaultContextSerializationRecord :: ContextSerializationRecord
defaultContextSerializationRecord = {id: "", prototype: Nothing, ctype: "", rollen: F.empty, externeProperties: PropertySerialization F.empty}

instance prettyPrintContextSerialization :: PrettyPrint ContextSerialization where
  prettyPrint' tab (ContextSerialization r) = "ContextSerialization " <> prettyPrint' (tab <> "  ") r

-----------------------------------------------------------
---- ROLSERIALIZATION
-----------------------------------------------------------
newtype RolSerialization = RolSerialization
  { id :: Maybe ID
  , properties :: PropertySerialization
  , binding :: Maybe ID
}

derive instance genericRolSerialization :: Generic RolSerialization _

derive newtype instance eqRolSerialization :: Eq RolSerialization

instance showRolSerialization :: Show RolSerialization where
  show (RolSerialization {properties, binding}) = "{ " <> show properties <> ", " <> show binding <> " }"

instance encodeRolSerialization :: Encode RolSerialization where
  encode = write

instance writeForeignRolSerialization :: WriteForeign RolSerialization where
  writeImpl (RolSerialization r) = write r

instance decodeRolSerialization :: Decode RolSerialization where
  decode = read'

instance readForeignRolSerialization :: ReadForeign RolSerialization where
  -- readImpl f = readString f >>= readJSON' >>= pure <<< RolSerialization
  readImpl = read' >=> pure <<< RolSerialization

instance prettyPrintRolSerialization :: PrettyPrint RolSerialization where
  prettyPrint' tab (RolSerialization r) = "RolSerialization " <> prettyPrint' (tab <> "  ") r

-----------------------------------------------------------
---- PROPERTYSERIALIZATION
-----------------------------------------------------------
newtype PropertySerialization = PropertySerialization (F.Object (Array Value))

derive instance genericPropertySerialization :: Generic PropertySerialization _

derive newtype instance eqPropertySerialization :: Eq PropertySerialization

instance showPropertySerialization :: Show PropertySerialization where
  show (PropertySerialization s) =  show s

instance encodePropertySerialization :: Encode PropertySerialization where
  encode = write

instance writeForeignPropertySerialization :: WriteForeign PropertySerialization where
  writeImpl (PropertySerialization p) = write p

instance decodePropertySerialization :: Decode PropertySerialization where
  decode = read'

instance readForeignPropertySerialization :: ReadForeign PropertySerialization where
  readImpl = read' >=> pure <<< PropertySerialization

instance prettyPrintPropertySerialization :: PrettyPrint PropertySerialization where
  prettyPrint' tab (PropertySerialization r) = "PropertySerialization " <> prettyPrint' (tab <> "  ") r
