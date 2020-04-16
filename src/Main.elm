module Main exposing (Model, Msg, init, main, update, view)

import Array
import Array2D exposing (Array2D)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type CellRevealedState
    = Default
    | Revealed Int


type alias CellState =
    { revealed : CellRevealedState
    , isBomb : Bool
    }


type alias Model =
    Array2D CellState


init : Model
init =
    Array2D.initialize 10
        10
        (\_ _ ->
            { revealed = Default
            , isBomb = False
            }
        )


type Msg
    = ClickRC Int Int
    | Reset
    | DebugRevealAll


revealCell : Array2D CellState -> Int -> Int -> CellState -> CellState
revealCell _ r c cell =
    { cell | revealed = Revealed (modBy 9 (r + c)) }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickRC r c ->
            let
                maybeCell =
                    Array2D.get r c model
            in
            case maybeCell of
                Nothing ->
                    model

                Just cell ->
                    case cell.revealed of
                        Default ->
                            Array2D.set
                                r
                                c
                                (revealCell model r c cell)
                                model

                        _ ->
                            model

        Reset ->
            init

        DebugRevealAll ->
            Array2D.indexedMap (\r c cell -> revealCell model r c cell) model


cellButtonView : List (Attribute msg) -> String -> Html msg
cellButtonView attrs str =
    span (class "gamecell" :: attrs) [ p [] [ text str ] ]


cellView : Int -> Int -> CellState -> Html Msg
cellView row col state =
    case state.revealed of
        Default ->
            cellButtonView [ onClick (ClickRC row col) ] ""

        Revealed 0 ->
            cellButtonView [ class "revealed" ] ""

        Revealed n ->
            cellButtonView [ class "revealed" ] (String.fromInt n)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Reset ] [ text "Reset" ]
        , button [ onClick DebugRevealAll ] [ text "DEBUG: Reveal All" ]
        , div [ class "gameboard" ]
            (List.indexedMap
                (\r row ->
                    div []
                        (List.indexedMap
                            (\c cell -> cellView r c cell)
                            (Array.toList row)
                        )
                )
                (Array.toList model.data)
            )
        ]
