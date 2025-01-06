module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)


initPlayer : Int -> Player
initPlayer id =
    Player id "" False


type alias Player =
    { id : Int
    , name : String
    , isActive : Bool
    }


type alias Model =
    { players : List Player
    , newPlayer : Player
    }


type Msg
    = SetName String
    | AddPlayer
    | ModifyPlayer Int Bool
    | DeletePlayer Int


init : Model
init =
    { players = []
    , newPlayer = initPlayer 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetName name ->
            model

        AddPlayer ->
            model

        DeletePlayer id ->
            model

        ModifyPlayer id status ->
            model


view : Model -> Html Msg
view model =
    h1 [] [ text "Elm Exercise: Players CRUD" ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
