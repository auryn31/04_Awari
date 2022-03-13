module Main exposing (main)

import Array exposing (Array, repeat)
import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { board : Array Int
    , playerOnesTurn : Bool
    , lastStonePit : Maybe Int
    , gameFinished : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = initArray 0 3 (repeat 14 0), playerOnesTurn = False, lastStonePit = Nothing, gameFinished = False }, Cmd.none )


type Msg
    = BoardClicked Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardClicked index ->
            if index == 7 || index == 0 || index < 7 && model.playerOnesTurn || index > 7 && not model.playerOnesTurn then
                ( model, Cmd.none )

            else
                let
                    nextBoard =
                        boardClicked index model.board

                    nextPlayer =
                        not model.playerOnesTurn

                    lastStoneInPitIndex =
                        lastStonePitPosition index model.board
                in
                ( { model | board = nextBoard, playerOnesTurn = nextPlayer, lastStonePit = lastStoneInPitIndex, gameFinished = calculateGameFinished nextBoard }, Cmd.none )


calculateGameFinished : Array Int -> Bool
calculateGameFinished board =
    Array.foldr (+) 0 board - Maybe.withDefault 0 (Array.get 0 board) - Maybe.withDefault 0 (Array.get 7 board) == 0


lastStonePitPosition : Int -> Array Int -> Maybe Int
lastStonePitPosition index board =
    Just (calcLastStonePosition index board (Maybe.withDefault 0 (Array.get index board)))


calcLastStonePosition : Int -> Array Int -> Int -> Int
calcLastStonePosition index board stones =
    if stones == 0 then
        index

    else
        calcLastStonePosition (incrementRotatingIndex index board) board (stones - 1)


view : Model -> Html Msg
view model =
    div [ style "margin" "4rem" ]
        [ h1 [] [ text "Board" ]
        , h3 []
            [ text
                ("It's your turn Player "
                    ++ (if model.playerOnesTurn then
                            " 1 "

                        else
                            " 0 "
                       )
                )
            ]
        , viewBoard model
        , viewPitPlayer 0 model.board
        , viewPitPlayer 1 model.board
        , div [] [ text ("Last stone was in pit with id: " ++ Maybe.withDefault "Not set yet" (Maybe.map String.fromInt model.lastStonePit)) ]
        , viewWinnerIfGameFinished model
        ]


viewWinnerIfGameFinished : Model -> Html msg
viewWinnerIfGameFinished model =
    if not model.gameFinished then
        div [] []

    else
        div [] [ text ("Winner is" ++ winnerPlayer model) ]


winnerPlayer : Model -> String
winnerPlayer model =
    if Maybe.withDefault 0 (Array.get 0 model.board) > Maybe.withDefault 0 (Array.get 7 model.board) then
        "Player 1"

    else if Maybe.withDefault 0 (Array.get 0 model.board) < Maybe.withDefault 0 (Array.get 7 model.board) then
        "Player 2"

    else
        "None"


viewBoard : Model -> Html Msg
viewBoard model =
    let
        boardGenerator =
            boardCard model.playerOnesTurn
    in
    div boardSyle
        (Array.toList <|
            Array.indexedMap boardGenerator model.board
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


boardCard : Bool -> Int -> Int -> Html Msg
boardCard playerOnesTurn index element =
    div (boardCardStyle playerOnesTurn index) [ text (String.fromInt element) ]


boardCardStyle : Bool -> Int -> List (Attribute Msg)
boardCardStyle playerOnesTurn index =
    if not playerOnesTurn && index < 7 || playerOnesTurn && index >= 7 then
        [ onClick (BoardClicked index)
        , style "grid-area" (areaFromIndex index)
        , style "cursor" "pointer"
        , style "background" "#36454F"
        , style "border-radius" "6px"
        , style "text-align" "center"
        , style "padding" "0.5rem"
        ]

    else
        [ style "grid-area" (areaFromIndex index)
        , style "background" "black"
        , style "border-radius" "6px"
        , style "text-align" "center"
        , style "padding" "0.5rem"
        ]


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
