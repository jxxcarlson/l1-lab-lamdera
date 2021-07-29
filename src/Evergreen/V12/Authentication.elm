module Evergreen.V12.Authentication exposing (..)

import Dict
import Evergreen.V12.Credentials
import Evergreen.V12.User


type alias Username =
    String


type alias UserData =
    { user : Evergreen.V12.User.User
    , credentials : Evergreen.V12.Credentials.Credentials
    }


type alias AuthenticationDict =
    Dict.Dict Username UserData
