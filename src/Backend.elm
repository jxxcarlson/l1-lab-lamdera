module Backend exposing (..)

import Authentication
import Backend.Cmd
import Backend.Update
import Data
import Dict
import Document exposing (Access(..))
import Lamdera exposing (ClientId, SessionId, sendToFrontend)
import List.Extra
import Random
import Time
import Token
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Time.every 1000 Tick
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!"

      -- RANDOM
      , randomSeed = Random.initialSeed 1234
      , uuidCount = 0
      , randomAtmosphericInt = Nothing
      , currentTime = Time.millisToPosix 0

      -- DATA
      , dataDict = Dict.empty

      -- USER
      , authenticationDict = Dict.empty

      -- DOCUMENTS
      , documents =
            [ Data.aboutCayatex
            , Data.docsNotFound
            , Data.notSignedIn
            , Data.foo
            ]
      }
    , Backend.Cmd.getRandomNumber
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        GotAtomsphericRandomNumber result ->
            Backend.Update.gotAtomsphericRandomNumber model result

        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        -- ADMIN
        RunTask ->
            let
                documents =
                    List.map (\doc -> { doc | slug = Just (Document.makeSlug model.currentTime doc) }) model.documents
            in
            ( { model | documents = documents }, sendToFrontend clientId (SendMessage <| "doc ids remapped") )

        SendUsers ->
            ( model, sendToFrontend clientId (GotUsers (Authentication.users model.authenticationDict)) )

        -- USER
        SignInOrSignUp username encryptedPassword ->
            case Dict.get username model.authenticationDict of
                Just userData ->
                    if Authentication.verify username encryptedPassword model.authenticationDict then
                        ( model
                        , Cmd.batch
                            [ sendToFrontend clientId (SendDocuments (List.filter (\doc -> doc.username == username) model.documents))
                            , sendToFrontend clientId (SendUser userData.user)
                            ]
                        )

                    else
                        ( model, sendToFrontend clientId (SendMessage <| "Sorry, password and username don't match") )

                Nothing ->
                    Backend.Update.setupUser model clientId username encryptedPassword

        -- DOCUMENTS
        GetUserDocuments username ->
            ( model, sendToFrontend clientId (SendDocuments (List.filter (\doc -> doc.username == username) model.documents)) )

        GetDocumentsWithQuery user (Query searchTerm) ->
            let
                username =
                    Maybe.map .username user

                docsFound =
                    Document.search user searchTerm model.documents
                        |> List.filter (\doc -> Just doc.username == username || doc.access == Public)
            in
            ( model, sendToFrontend clientId (SendDocuments docsFound) )

        RegisterNewDocument doc_ ->
            let
                { token, seed } =
                    Token.get model.randomSeed

                doc =
                    { doc_
                        | id = token
                        , created = model.currentTime
                        , modified = model.currentTime
                        , slug = Just (Document.makeSlug model.currentTime doc_)
                    }

                newDocuments =
                    doc :: model.documents

                message =
                    "Registered document: " ++ doc.title ++ "(" ++ doc.username ++ ")"
            in
            ( { model | randomSeed = seed, documents = newDocuments }
            , Cmd.batch
                [ sendToFrontend clientId (SendDocument doc)
                , sendToFrontend clientId (SendMessage message)
                ]
            )

        SaveDocument document ->
            let
                newDocuments =
                    List.Extra.setIf (\doc -> doc.id == document.id) document model.documents
            in
            ( { model | documents = newDocuments }, sendToFrontend clientId (SendMessage ("Saved document: " ++ document.title)) )

        DeleteDocumentById id ->
            ( { model | documents = List.filter (\doc -> doc.id /= id) model.documents }, Cmd.none )

        GetDocumentById id ->
            case List.head (List.filter (\doc -> doc.id == id) model.documents) of
                Nothing ->
                    ( model
                    , sendToFrontend clientId (SendMessage <| "Could not find document: " ++ id)
                    )

                Just doc ->
                    ( model
                    , Cmd.batch
                        [ sendToFrontend clientId (SendDocument doc)
                        ]
                    )

        GetDocumentBySlug slug ->
            case List.head (List.filter (\doc -> doc.slug == Just slug) model.documents) of
                Nothing ->
                    ( model
                    , sendToFrontend clientId (SendMessage <| "Could not find document: " ++ slug)
                    )

                Just doc ->
                    ( model
                    , Cmd.batch
                        [ sendToFrontend clientId (SendDocument doc)
                        ]
                    )

        GetDocumentByIdForGuest id ->
            if String.left 2 id /= "g/" then
                ( model, Cmd.none )

            else
                case List.head (List.filter (\doc -> doc.id == String.dropLeft 2 id) model.documents) of
                    Nothing ->
                        ( model
                        , sendToFrontend clientId (SendMessage <| "Could not find document: " ++ String.dropLeft 2 id)
                        )

                    Just doc ->
                        ( model
                        , Cmd.batch
                            [ sendToFrontend clientId (SendDocument doc)
                            , sendToFrontend clientId (SendMessage "Signed in as guest")
                            , sendToFrontend clientId LoginGuest
                            ]
                        )


idMessage model =
    "ids: " ++ (List.map .id model.documents |> String.join ", ")
