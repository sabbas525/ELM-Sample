-- Fetch players from backend on load
-- Delete player from backend first then delete player from frontend on success
-- modify player from backend first then modify player from frontend on success
-- modify player from backend first then modify player from frontend on success
-- add player to backend first then add player to frontend on success


module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, field, map3)
import Json.Encode as Encode


type alias Player =
    { id : Int
    , name : String
    , isActive : Bool
    }


type alias Model =
    { players : List Player
    , newPlayer : Player
    , baseUrl : String
    , reqStatus : String
    }


type Msg
    = SetName String
    | FetchPlayers (Result Http.Error (List Player))
    | PutPlayerReq Int Bool
    | ModifyPlayer (Result Http.Error Player)
    | PostPlayerReq
    | AddPlayer (Result Http.Error Player)
    | DeletePlayerReq Int
    | DeletePlayer Int (Result Http.Error ())


playerEncoder : Player -> Encode.Value
playerEncoder player =
    Encode.object
        [ ( "id", Encode.int player.id )
        , ( "name", Encode.string player.name )
        , ( "isActive", Encode.bool player.isActive )
        ]


playerDecoder : Decoder Player
playerDecoder =
    map3 Player (field "id" Decode.int) (field "name" Decode.string) (field "isActive" Decode.bool)


playersDecoder : Decoder (List Player)
playersDecoder =
    Decode.list playerDecoder


fetchPlayers : String -> Cmd Msg
fetchPlayers url = Cmd.none

postPlayerReq : String -> Player -> Cmd Msg
postPlayerReq url player = Cmd.none


deletePlayerReq : String -> Int -> Cmd Msg
deletePlayerReq url id = Cmd.none


putPlayerReq : String -> Player -> Cmd Msg
putPlayerReq url player = Cmd.none


listLast : List a -> Maybe a
listLast list =
    List.head <| List.reverse list


initPlayer : Int -> Player
initPlayer id =
    Player id "" False


initModel : Model
initModel =
    { players = []
    , newPlayer = initPlayer 0
    , baseUrl = "http://localhost:3001/api/players/"
    , reqStatus = "Loading..."
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , fetchPlayers initModel.baseUrl
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName word ->
            ( model, Cmd.none )

        FetchPlayers data ->
            ( model, Cmd.none )

        PostPlayerReq ->
            ( model, Cmd.none )

        AddPlayer data ->
            ( model, Cmd.none )

        PutPlayerReq id status ->
            ( model, Cmd.none )

        ModifyPlayer data ->
            ( model, Cmd.none )

        DeletePlayerReq id ->
            ( model, Cmd.none )

        DeletePlayer id data ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    h1 [] [ text "Elm Exercise: Players CRUD2" ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
