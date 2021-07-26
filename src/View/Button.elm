module View.Button exposing
    ( adminPopup
    , deleteDocument
    , export
    , exportToMarkown
    , fetchDocuments
    , getDocument
    , getUsers
    , help
    , linkTemplate
    , newDocument
    , printToPDF
    , runTask
    , signIn
    , signOut
    , startupHelp
    , toggleAccess
    , toggleEditor
    )

import Config
import Document exposing (Access(..))
import Element as E exposing (Element)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Types exposing (..)
import View.Color as Color
import View.Style
import View.Utility



-- TEMPLATES


buttonTemplate : List (E.Attribute msg) -> msg -> String -> Element msg
buttonTemplate attrList msg label_ =
    E.row ([ View.Style.bgGray 0.2, E.pointer, E.mouseDown [ Background.color Color.darkRed ] ] ++ attrList)
        [ Input.button View.Style.buttonStyle
            { onPress = Just msg
            , label = E.el [ E.centerX, E.centerY, Font.size 14 ] (E.text label_)
            }
        ]


linkTemplate : msg -> E.Color -> String -> Element msg
linkTemplate msg fontColor label_ =
    E.row [ E.pointer, E.mouseDown [ Background.color Color.paleBlue ] ]
        [ Input.button linkStyle
            { onPress = Just msg
            , label = E.el [ E.centerX, E.centerY, Font.size 14, Font.color fontColor ] (E.text label_)
            }
        ]


linkStyle =
    [ Font.color (E.rgb255 255 255 255)
    , E.paddingXY 8 2
    ]



-- UI


toggleEditor model =
    let
        title =
            if model.showEditor then
                "Hide Editor"

            else
                "Show Editor"
    in
    buttonTemplate [ Background.color Color.darkBlue ] ToggleEditor title



-- USER


signOut username =
    buttonTemplate [] SignOut username



-- DOCUMENT


deleteDocument : FrontendModel -> Element FrontendMsg
deleteDocument model =
    let
        title =
            case model.documentDeleteState of
                WaitingForDeleteAction ->
                    "Delete"

                DocumentDeletePending ->
                    "Delete forever?"

        bg =
            case model.documentDeleteState of
                WaitingForDeleteAction ->
                    Background.color (E.rgb255 0 0 0)

                DocumentDeletePending ->
                    Background.color Color.darkRed

        action =
            case model.documentDeleteState of
                WaitingForDeleteAction ->
                    ChangeDocumentDeleteStateFrom WaitingForDeleteAction

                DocumentDeletePending ->
                    ChangeDocumentDeleteStateFrom DocumentDeletePending
    in
    buttonTemplate [ bg ] action title


exportToMarkown : Element FrontendMsg
exportToMarkown =
    buttonTemplate [] ExportToMarkdown "Export to Markdown"


export : Element FrontendMsg
export =
    buttonTemplate [] Export "Export"


printToPDF : FrontendModel -> Element FrontendMsg
printToPDF model =
    case model.printingState of
        PrintWaiting ->
            buttonTemplate [ View.Utility.elementAttribute "title" "Generate PDF" ] PrintToPDF "PDF"

        PrintProcessing ->
            E.el [ Font.size 14, E.padding 8, E.height (E.px 30), Background.color Color.blue, Font.color Color.white ] (E.text "Please wait ...")

        PrintReady ->
            E.link
                [ Font.size 14
                , Background.color Color.white
                , E.paddingXY 8 8
                , Font.color Color.blue
                , Events.onClick (ChangePrintingState PrintWaiting)
                , View.Utility.elementAttribute "target" "_blank"
                ]
                { url = Config.pdfServer ++ "/pdf/" ++ model.currentDocument.id, label = E.el [] (E.text "Click for PDF") }


fetchDocuments : String -> Element FrontendMsg
fetchDocuments query =
    buttonTemplate [] (FetchDocuments (Query query)) "Fetch"


newDocument : Element FrontendMsg
newDocument =
    buttonTemplate [] NewDocument "New"


help =
    buttonTemplate [] (Help Config.helpDocumentId) "Help"


startupHelp =
    buttonTemplate [] (Help Config.startupHelpDocumentId) "Help"



-- USER


getDocument : Element FrontendMsg
getDocument =
    buttonTemplate [] (AskFoDocumentById "aboutCYT") "Get document"


signIn : Element FrontendMsg
signIn =
    buttonTemplate [] SignIn "Sign in | Sign up"


toggleAccess : FrontendModel -> Element FrontendMsg
toggleAccess model =
    let
        label =
            case model.currentDocument.access of
                Public ->
                    "Public"

                Private ->
                    "Private"

                Shared _ ->
                    "Shared"
    in
    buttonTemplate [] ToggleAccess label



--nextPopupState : FrontendModel -> PopupWindow -> PopupStatus -> PopupStatus
--nextPopupState model popupWindow_ popupStatus =
--    case model.popupStatus of
--        PopupClosed ->
--            PopupOpen popupWindow_
--
--        PopupOpen popupWindow_ ->
--            PopupClosed
--
--        PopupOpen _ ->
--            PopupOpen popupWindow_
--
----nextState =
----    case model.popupStatus of
----        PopupClosed ->
----            PopupOpen ChatPopup
----
----        PopupOpen ChatPopup ->
----            PopupClosed
----
----        PopupOpen _ ->
----            PopupOpen ChatPopup
-- ADMIN


runTask : Element FrontendMsg
runTask =
    buttonTemplate [] AdminRunTask "Run Task"


adminPopup : FrontendModel -> Element FrontendMsg
adminPopup model =
    let
        nextState : PopupStatus
        nextState =
            case model.popupStatus of
                PopupClosed ->
                    PopupOpen AdminPopup

                PopupOpen AdminPopup ->
                    PopupClosed

        --PopupOpen _ ->
        --    PopupOpen AdminPopup
        isVisible =
            Maybe.map .username model.currentUser == Just Config.administrator
    in
    View.Utility.showIf isVisible <| buttonTemplate [] (ChangePopupStatus nextState) "Admin"


getUsers =
    buttonTemplate [] GetUsers "Get Users"



--Widget.titledButton
--    { label = "Toggle Chat"
--    , title = "Toggle chat (^C)"
--    , action = ChangePopupStatus nextState
--    , style = Style.headerButton
--    }
