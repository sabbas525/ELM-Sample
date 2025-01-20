-- Fetch players from end point on load
-- Update the id from the fetched players
-- Add player to the end of the list


module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, field, map3)


type alias Player =
    { id : Int
    , name : String
    , isActive : Bool
    }


type alias Model =
    { players : List Player
    , newPlayer : Player
    , reqStatus : String
    }


type Msg
    = SetName String
    | ModifyPlayer Int Bool
    | AddPlayer
    | DeletePlayer Int
    | FetchPlayers (Result Http.Error (List Player))


playerDecoder : Decoder Player
playerDecoder =
    map3 Player (field "id" Decode.int) (field "name" Decode.string) (field "isActive" Decode.bool)


playersDecoder : Decoder (List Player)
playersDecoder =
    Decode.list playerDecoder


fetchPlayers : String -> Cmd Msg
fetchPlayers url = 
    Http.get { url = url, expect = Http.expectJson FetchPlayers playersDecoder }



listLast : List a -> Maybe a
listLast list =
    List.head <| List.reverse list


initPlayer : Int -> Player
initPlayer id =
    Player id "" False


init : () -> ( Model, Cmd Msg )
init _ =
    ( { 
        players = []
      , newPlayer = initPlayer 0
      , reqStatus = "Loading..."
    }
    , fetchPlayers "http://localhost:3001/api/players/"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName word ->
            let
                updatedNewPlayer = model.newPlayer
                    |> (\player -> { player | name = word })
            in
            ( { model | newPlayer = updatedNewPlayer }, Cmd.none )

        AddPlayer ->
            let
                newId =
                    List.length model.players + 1
                newPlayer =
                    { id = newId, name = model.newPlayer.name, isActive = False}
                updatedPlayers =
                    model.players ++ [ newPlayer ]
            in
            ( { model
                | players = updatedPlayers
                , newPlayer = initPlayer (newId + 1)
            } , Cmd.none )

        DeletePlayer id ->
            let
                updatedPlayers =
                    List.filter (\player -> player.id /= id) model.players
            in
            ( { model | players = updatedPlayers }, Cmd.none )

        ModifyPlayer id status ->
            let
                updatedPlayers =
                    List.map
                        (\player ->
                            if player.id == id then
                                { player | isActive = status }
                            else
                                player
                        )
                        model.players
            in
            ( { model | players = updatedPlayers }, Cmd.none )

        FetchPlayers data ->
            case data of
                Ok fetchedPlayers ->
                    let
                        updatedId =
                            case listLast fetchedPlayers of
                                Just lastPlayer ->
                                    lastPlayer.id + 1
                                Nothing ->
                                    0
                    in
                    ( { model
                        | players = fetchedPlayers
                        , newPlayer = initPlayer updatedId
                        , reqStatus = ""
                    }, Cmd.none )

                Err _ ->
                    ( { model | reqStatus = "An error has occurred!!!" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "elm exercise: players Fetch" ]
        , Html.form [ id "submit-player", onSubmit AddPlayer ]
            [ input
                [ id "input-player"
                , type_ "text"
                , placeholder "Enter new player"
                , value model.newPlayer.name
                , onInput SetName
                ]
                []
            , button [ id "btn-add", type_ "submit" ] [ text "Add" ]
            ]
        , ol [ id "players-list" ]
            (List.map playerView model.players)
        , div [ id "request-status" ] [ text model.reqStatus ]
        ]


playerView : Player -> Html Msg
playerView player =
        li [ id ("player-" ++ String.fromInt player.id) ]
            [ div [ class "player-name" ] [ text player.name ]
            , label [ class "player-status" ]
                [ input
                    [ type_ "checkbox"
                    , class "player-status"
                    , checked player.isActive
                    , onCheck (\isActive -> ModifyPlayer player.id isActive)
                    ]
                    []
                , span [ class "checkmark" ] []
                , text (if player.isActive then "Active" else "Not active")
                ]
            , button
                [ class "btn-delete"
                , onClick (DeletePlayer player.id)
                ]
                [ text "Delete" ]
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
