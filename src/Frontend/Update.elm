module Frontend.Update exposing
    ( deleteDocument
    , newDocument
    , updateCurrentDocument
    , updateWithViewport
    )

import Document exposing (Document)
import Lamdera exposing (sendToBackend)
import List.Extra
import Types exposing (..)


updateWithViewport vp model =
    let
        w =
            round vp.viewport.width

        h =
            round vp.viewport.height
    in
    ( { model
        | windowWidth = w
        , windowHeight = h
      }
    , Cmd.none
    )


newDocument model =
    case model.currentUser of
        Nothing ->
            ( model, Cmd.none )

        Just user ->
            if user.username == "guest" then
                ( model, Cmd.none )

            else
                let
                    emptyDoc =
                        Document.empty

                    doc =
                        { emptyDoc
                            | title = "New Document"
                            , author = user.realname
                            , username = user.username
                            , content = "[title New Document]"
                        }
                in
                ( model, sendToBackend (RegisterNewDocument doc) )


deleteDocument : FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
deleteDocument model =
    if Just model.currentDocument.username /= Maybe.map .username model.currentUser then
        ( model, Cmd.none )

    else
        ( { model
            | currentDocument = Document.deletedMessage model.currentDocument.title
            , documents = List.filter (\doc -> doc.id /= model.currentDocument.id) model.documents
            , documentDeleteState = WaitingForDeleteAction
          }
        , sendToBackend (DeleteDocumentById model.currentDocument.id)
        )


updateCurrentDocument : Document -> FrontendModel -> FrontendModel
updateCurrentDocument doc model =
    { model | currentDocument = doc, documents = List.Extra.setIf (\d -> d.id == doc.id) doc model.documents }
