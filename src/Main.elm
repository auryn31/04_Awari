module Main exposing (main)

import Array exposing (Array, repeat)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Array Int
    , playersTurn : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (initArray 0 36 (repeat 14 0)) 0, Cmd.none )


type Msg
    = BoardClicked Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardClicked index ->
            if index == 7 || index == 0 || index < 7 && model.playersTurn == 1 || index > 7 && model.playersTurn == 0 then
                ( model, Cmd.none )

            else if model.playersTurn == 0 then
                ( { model | board = boardClicked index model.board, playersTurn = 1 }, Cmd.none )

            else
                ( { model | board = boardClicked index model.board, playersTurn = 0 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ style "margin" "4rem" ]
        [ h1 [] [ text "Board" ]
        , h3 [] [ text ("It's your turn Player " ++ String.fromInt model.playersTurn) ]
        , viewBoard model
        , viewPitPlayer 0 model.board
        , viewPitPlayer 1 model.board
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    div boardSyle
        (Array.toList <|
            Array.indexedMap boardCard model.board
        )


boardSyle : List (Attribute msg)
boardSyle =
    [ style "display" "flex"
    , style "gap" "1rem"
    , style "display" "grid"
    , style "grid-template-columns" "repeat(8, 1fr)"
    , style "align-items" "center"
    , style "grid-template-areas" """
    'a b c d e f g h'
    'a n m l k j i h'
    """
    ]


areaFromIndex : Int -> String
areaFromIndex index =
    case index of
        0 ->
            "a"

        1 ->
            "b"

        2 ->
            "c"

        3 ->
            "d"

        4 ->
            "e"

        5 ->
            "f"

        6 ->
            "g"

        7 ->
            "h"

        8 ->
            "i"

        9 ->
            "j"

        10 ->
            "k"

        11 ->
            "l"

        12 ->
            "m"

        13 ->
            "n"

        i ->
            String.fromInt i


boardCard : Int -> Int -> Html Msg
boardCard index element =
    div [ onClick (BoardClicked index), style "grid-area" (areaFromIndex index) ] [ text (String.fromInt element) ]


viewPitPlayer : Int -> Array Int -> Html Msg
viewPitPlayer player board =
    if player == 0 then
        div []
            [ text ("Player " ++ String.fromInt player)
            , text
                (" Pit contains: "
                    ++ Maybe.withDefault "-"
                        (Maybe.map
                            String.fromInt
                            (Array.get 0 board)
                        )
                    ++ " Stones"
                )
            ]

    else
        div []
            [ text ("Player " ++ String.fromInt player)
            , text
                (" Pit contains: "
                    ++ Maybe.withDefault "-"
                        (Maybe.map
                            String.fromInt
                            (Array.get 7 board)
                        )
                    ++ " Stones"
                )
            ]


initArray : Int -> Int -> Array Int -> Array Int
initArray index stones array =
    if stones == 0 then
        array

    else if index == 0 || index == 7 then
        initArray (index + 1) stones array

    else
        initArray (index + 1) (stones - 3) (Array.set index 3 array)


boardClicked : Int -> Array Int -> Array Int
boardClicked index board =
    if index == 0 || index == 7 then
        board

    else
        let
            stonesLeft =
                Maybe.withDefault 0 (Array.get index board)

            startBoard =
                Array.set index 0 board
        in
        placeStones (incrementRotatingIndex index startBoard) stonesLeft startBoard


incrementRotatingIndex : Int -> Array Int -> Int
incrementRotatingIndex currentIndex board =
    if 0 == currentIndex then
        Array.length board - 1

    else
        currentIndex - 1


placeStones : Int -> Int -> Array Int -> Array Int
placeStones index stonesLeft board =
    if stonesLeft == 0 then
        board

    else
        let
            oldStones =
                Maybe.withDefault 0 (Array.get index board)
        in
        placeStones (incrementRotatingIndex index board) (stonesLeft - 1) (Array.set index (oldStones + 1) board)
