module Evergreen.Migrate.V2 exposing (..)

import Evergreen.V1.Document
import Evergreen.V2.Document
import Evergreen.V1.Data
import Evergreen.V2.Data
import Evergreen.V1.User
import Evergreen.V2.User
import Evergreen.V1.Credentials
import Evergreen.V2.Credentials
import Evergreen.V1.Authentication
import Evergreen.V2.Authentication
import Evergreen.V1.Types as Old
import Evergreen.V2.Types as New
import Lamdera.Migrations exposing (..)
import Dict


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated ( {
          message= old.message
        , currentTime = old.currentTime

        -- RANDOM
        , randomSeed = old.randomSeed
        , uuidCount = old.uuidCount
        , randomAtmosphericInt  = old.randomAtmosphericInt

       -- DATA
       , dataDict = dataDictIdentity old.dataDict

       -- USER
       , authenticationDict = authenticationDictIdentity  old.authenticationDict

        -- DOCUMENT
        , documents  = List.map updateDoc old.documents
        }
         ,Cmd.none
       )



dataDictIdentity : Evergreen.V1.Data.DataDict -> Evergreen.V2.Data.DataDict
dataDictIdentity old =
     Dict.fromList (List.map (\(name, dataFile) -> (name, dataFileIdentity dataFile))  (Dict.toList old) )

dataFileIdentity : Evergreen.V1.Data.DataFile -> Evergreen.V2.Data.DataFile
dataFileIdentity old =
        { data = List.map datumIdentity old.data
        , username = old.username
        , creationData = old.creationData
        , modificationData = old.modificationData
        }


datumIdentity : Evergreen.V1.Data.Datum -> Evergreen.V2.Data.Datum
datumIdentity old =
        { id = old.id
        , title = old.title
        , username = old.username
        , content = old.content
        , tags = old.tags
        , creationData = old.creationData
        , modificationData = old.modificationData
        }



userIdentity : Evergreen.V1.User.User ->  Evergreen.V2.User.User
userIdentity old =
     {   username = old.username
        , id = old.id
        , realname = old.realname
        , email = old.email
        , created = old.created
        , modified = old.modified
        }

credentialsIdentity : Evergreen.V1.Credentials.Credentials -> Evergreen.V2.Credentials.Credentials
credentialsIdentity (Evergreen.V1.Credentials.V1 a b)  = (Evergreen.V2.Credentials.V1 a b)

userDataIdentity : Evergreen.V1.Authentication.UserData ->  Evergreen.V2.Authentication.UserData
userDataIdentity {user, credentials}  = {user = userIdentity user, credentials = credentialsIdentity credentials}

authenticationDictIdentity : Evergreen.V1.Authentication.AuthenticationDict -> Evergreen.V2.Authentication.AuthenticationDict
authenticationDictIdentity old =
    Dict.fromList (List.map (\(name, data) -> (name, userDataIdentity data))  (Dict.toList old) )

updateDoc : Evergreen.V1.Document.Document -> Evergreen.V2.Document.Document
updateDoc old =
   { title = old.title
       , author  = old.author
       , username = old.username
       , id = old.id
       , created = old.created
       , modified = old.modified
       , tags = old.tags
       , content = old.content
       , access = accessIdentity old.access
       , slug = Nothing
       }


accessIdentity : Evergreen.V1.Document.Access -> Evergreen.V2.Document.Access
accessIdentity old =
    case old of
        Evergreen.V1.Document.Public -> Evergreen.V2.Document.Public
        Evergreen.V1.Document.Private -> Evergreen.V2.Document.Private
        Evergreen.V1.Document.Shared { canRead, canWrite } -> Evergreen.V2.Document.Shared { canRead = canRead, canWrite = canWrite}


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged