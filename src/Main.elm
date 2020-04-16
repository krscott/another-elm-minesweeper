module Main exposing (Model, Msg, init, main, update, view)

import Array
import Array2D exposing (Array2D)
import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled)
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
                                { cell | revealed = Revealed 0 }
                                model

                        _ ->
                            model

        Reset ->
            init


cellView : Int -> Int -> CellState -> Html Msg
cellView row col state =
    case state.revealed of
        Default ->
            button [ onClick (ClickRC row col) ] [ text "?" ]

        Revealed n ->
            button [ disabled True ] [ text (String.fromInt n) ]


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Reset ] [ text "Reset" ]
        , table []
            (List.indexedMap
                (\r row ->
                    tr []
                        (List.indexedMap
                            (\c cell -> td [] [ cellView r c cell ])
                            (Array.toList row)
                        )
                )
                (Array.toList model.data)
            )
        ]
