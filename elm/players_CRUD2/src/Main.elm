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
fetchPlayers url = 
    Http.get { url = url, expect = Http.expectJson FetchPlayers playersDecoder }


postPlayerReq : String -> Player -> Cmd Msg
postPlayerReq url player = 
    Http.post
        { url = url
        , body = Http.jsonBody (playerEncoder player)
        , expect = Http.expectJson AddPlayer playerDecoder
        }


deletePlayerReq : String -> Int -> Cmd Msg
deletePlayerReq url id = 
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever (\_ -> DeletePlayer id (Ok ()))
        , timeout = Nothing
        , tracker = Nothing
        }


putPlayerReq : String -> Player -> Cmd Msg
putPlayerReq url player = 
    Http.request
        { method = "PUT"
        , headers = []
        , url = url ++ String.fromInt player.id
        , body = Http.jsonBody (playerEncoder player)
        , expect = Http.expectJson ModifyPlayer playerDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


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
            let
                updatedNewPlayer = model.newPlayer
                    |> (\player -> { player | name = word })
            in
            ( { model | newPlayer = updatedNewPlayer }, Cmd.none )


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


        PostPlayerReq ->
            ( model, postPlayerReq model.baseUrl model.newPlayer )

        AddPlayer data ->
            case data of
                Ok newPlayer ->
                    let
                        updatedPlayers =
                            model.players ++ [ newPlayer ]
                    in
                    ( { model
                        | players = updatedPlayers
                        , newPlayer = initPlayer (newPlayer.id + 1)
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | reqStatus = "An error has occurred!!!" }, Cmd.none )

        PutPlayerReq id status ->
            let
                maybePlayer =
                    List.head (List.filter (\p -> p.id == id) model.players)

                updatedPlayer =
                    Maybe.map (\p -> { p | isActive = status }) maybePlayer
            in
            case updatedPlayer of
                Just player ->
                    ( model, putPlayerReq model.baseUrl player )

                Nothing ->
                    ( model, Cmd.none )

        ModifyPlayer data ->
            case data of
                Ok updatedPlayer ->
                    ( { model
                        | players =
                            List.map (\p -> if p.id == updatedPlayer.id then updatedPlayer else p) model.players
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | reqStatus = "An error has occurred!!!" }, Cmd.none )

        DeletePlayerReq id ->
            ( model, deletePlayerReq model.baseUrl id )

        DeletePlayer id data ->
            case data of
                Ok _ ->
                    ( { model | players = List.filter (\p -> p.id /= id) model.players }, Cmd.none )

                Err _ ->
                    ( { model | reqStatus = "An error has occurred!!!" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "elm exercise: players Fetch" ]
        , Html.form [ id "submit-player", onSubmit PostPlayerReq ]
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
                    , onCheck (PutPlayerReq player.id)
                    ]
                    []
                , span [ class "checkmark" ] []
                , text (if player.isActive then "Active" else "Not active")
                ]
            , button
                [ class "btn-delete"
                , onClick (DeletePlayerReq player.id)
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
