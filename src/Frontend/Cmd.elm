module Frontend.Cmd exposing (saveDocument, setupWindow)

import Browser.Dom as Dom
import Document exposing (Document)
import Lamdera exposing (sendToBackend)
import Task
import Types exposing (FrontendModel, FrontendMsg(..), ToBackend(..))


setupWindow : Cmd FrontendMsg
setupWindow =
    Task.perform GotViewport Dom.getViewport


saveDocument : FrontendModel -> Document -> Cmd FrontendMsg
saveDocument model document =
    if Maybe.map .username model.currentUser == Just document.username then
        sendToBackend (SaveDocument document)

    else
        Cmd.none
