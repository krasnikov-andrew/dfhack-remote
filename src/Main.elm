port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


type alias Screen =
    { height : Int
    , width : Int
    , tiles : List Tile
    }


type alias Tile =
    { character : Int
    , background : Int
    , foreground : Int
    }


port sendMessage : () -> Cmd msg


port messageReceiver : (Screen -> msg) -> Sub msg



-- MODEL


type alias Model =
    { screen : Screen
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screen = Screen 0 0 [] }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Recv Screen



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Recv message ->
            ( { model | screen = message }
            , sendMessage ()
            )



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv



-- VIEW


numberToColour : Int -> String
numberToColour number =
    case number of
        0 ->
            "#000000"

        1 ->
            "#000080"

        2 ->
            "#008000"

        3 ->
            "#008080"

        4 ->
            "#800000"

        5 ->
            "#800080"

        6 ->
            "#808000"

        7 ->
            "#C0C0C0"

        8 ->
            "#808080"

        9 ->
            "#0000FF"

        10 ->
            "#00FF00"

        11 ->
            "#00FFFF"

        12 ->
            "#FF0000"

        13 ->
            "#FF00FF"

        14 ->
            "#FFFF00"

        15 ->
            "#FFFFFF"

        _ ->
            "ERROR"


viewTile : Tile -> Html msg
viewTile tile =
    div
        [ class "tile"
        , style "color" (numberToColour tile.foreground)
        , style "background-color" (numberToColour tile.background)
        ]
        [ Char.fromCode tile.character
            |> String.fromChar
            |> text
        ]


getRowsHelp : List Tile -> Int -> List (List Tile)
getRowsHelp tiles height =
    let
        first =
            List.take height tiles
    in
    if List.length first == 0 then
        []

    else
        first :: getRowsHelp (List.drop height tiles) height


getRows : Screen -> List (List Tile)
getRows screen =
    getRowsHelp screen.tiles screen.height


viewRow : List Tile -> Html msg
viewRow row =
    div [ class "row" ] (List.map viewTile row)


viewScreen : Screen -> Html msg
viewScreen screen =
    let
        rows =
            getRows screen
    in
    div [ class "screen" ] (List.map (lazy viewRow) rows)


view : Model -> Html Msg
view model =
    div []
        [ lazy viewScreen model.screen ]