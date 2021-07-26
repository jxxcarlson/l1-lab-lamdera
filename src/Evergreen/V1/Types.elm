module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Evergreen.V1.Authentication
import Evergreen.V1.Data
import Evergreen.V1.Document
import Evergreen.V1.User
import Http
import Random
import Time
import Url


type PopupWindow
    = AdminPopup


type PopupStatus
    = PopupOpen PopupWindow
    | PopupClosed


type PrintingState
    = PrintWaiting
    | PrintProcessing
    | PrintReady


type DocumentDeleteState
    = WaitingForDeleteAction
    | DocumentDeletePending


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , message : String
    , users : List Evergreen.V1.User.User
    , currentUser : Maybe Evergreen.V1.User.User
    , inputUsername : String
    , inputPassword : String
    , windowWidth : Int
    , windowHeight : Int
    , popupStatus : PopupStatus
    , showEditor : Bool
    , currentDocument : Evergreen.V1.Document.Document
    , documents : List Evergreen.V1.Document.Document
    , inputSearchKey : String
    , printingState : PrintingState
    , documentDeleteState : DocumentDeleteState
    , counter : Int
    }


type alias BackendModel =
    { message : String
    , currentTime : Time.Posix
    , randomSeed : Random.Seed
    , uuidCount : Int
    , randomAtmosphericInt : Maybe Int
    , dataDict : Evergreen.V1.Data.DataDict
    , authenticationDict : Evergreen.V1.Authentication.AuthenticationDict
    , documents : List Evergreen.V1.Document.Document
    }


type SearchTerm
    = Query String


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | GotNewWindowDimensions Int Int
    | GotViewport Browser.Dom.Viewport
    | SetViewPortForElement (Result Browser.Dom.Error ( Browser.Dom.Element, Browser.Dom.Viewport ))
    | ChangePopupStatus PopupStatus
    | ToggleEditor
    | SignIn
    | SignOut
    | InputUsername String
    | InputPassword String
    | AdminRunTask
    | GetUsers
    | InputText String
    | InputSearchKey String
    | NewDocument
    | ChangeDocumentDeleteStateFrom DocumentDeleteState
    | AskFoDocumentById String
    | FetchDocuments SearchTerm
    | ExportToMarkdown
    | Export
    | PrintToPDF
    | GotPdfLink (Result Http.Error String)
    | ChangePrintingState PrintingState
    | FinallyDoCleanPrintArtefacts String
    | ToggleAccess
    | Help String


type ToBackend
    = NoOpToBackend
    | RunTask
    | SendUsers
    | SignInOrSignUp String String
    | SaveDocument Evergreen.V1.Document.Document
    | DeleteDocumentById String
    | GetUserDocuments String
    | GetDocumentsWithQuery (Maybe Evergreen.V1.User.User) SearchTerm
    | GetDocumentById String
    | GetDocumentByIdForGuest String
    | RegisterNewDocument Evergreen.V1.Document.Document


type BackendMsg
    = NoOpBackendMsg
    | GotAtomsphericRandomNumber (Result Http.Error String)


type ToFrontend
    = NoOpToFrontend
    | GotUsers (List Evergreen.V1.User.User)
    | SendUser Evergreen.V1.User.User
    | LoginGuest
    | SendDocument Evergreen.V1.Document.Document
    | SendDocuments (List Evergreen.V1.Document.Document)
    | SendMessage String
