module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)


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


initPlayer : Int -> Player
initPlayer id =
    Player id "" False


init : Model
init =
    { players = []
    , newPlayer = initPlayer 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetName name ->
            let
                updatedNewPlayer = model.newPlayer
                    |> (\player -> { player | name = name })
            in
            { model | newPlayer = updatedNewPlayer }    
            
        AddPlayer ->
            let
                newId =
                    case List.head (List.reverse model.players) of
                        Just lastplayer -> 
                            lastplayer.id + 1

                        Nothing -> 
                            0
                newPlayer =
                    { id = newId, name = model.newPlayer.name, isActive = False}
                
            in
            { model
                | players = model.players ++ [ newPlayer ]
                , newPlayer = initPlayer (newId + 1)
            }

        DeletePlayer id ->
            let
                updatedPlayers =
                    List.filter (\player -> player.id /= id) model.players
            in
            { model | players = updatedPlayers }

        ModifyPlayer id status ->
            let
                updatedPlayerstatus =
                    List.map
                        (\player ->
                            if player.id == id then
                                { player | isActive = status }
                            else
                                player
                        )
                        model.players
            in
            { model | players = updatedPlayerstatus }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "elm exercise: players CRUD" ]
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
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
