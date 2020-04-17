module Main exposing (main)

-- import Random.List exposing (shuffle)

import Array
import Array2D exposing (Array2D)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random exposing (generate)
import Shuffle exposing (shuffle)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type CellRevealedState
    = Default
    | Revealed Int
    | RevealedMine


type alias CellState =
    { revealed : CellRevealedState
    , isMine : Bool
    }


defaultCellState : CellState
defaultCellState =
    { revealed = Default, isMine = False }


type alias Model =
    Array2D CellState


initModel : Int -> Int -> Model
initModel rows cols =
    Array2D.initialize
        rows
        cols
        (\_ _ ->
            { revealed = Default
            , isMine = False
            }
        )


init : () -> ( Model, Cmd Msg )
init _ =
    update Reset (initModel 10 15)



-- UPDATE


allCoords : Array2D a -> List ( Int, Int )
allCoords grid =
    let
        rows =
            Array2D.rows grid

        cols =
            Array2D.columns grid
    in
    List.range 0 (rows - 1)
        |> List.map
            (\r ->
                List.range 0 (cols - 1)
                    |> List.map (\c -> ( r, c ))
            )
        |> List.concat


setMine : Int -> Int -> Array2D CellState -> Array2D CellState
setMine r c grid =
    let
        newCell =
            case Array2D.get r c grid of
                Just cell ->
                    { cell | isMine = True }

                Nothing ->
                    { revealed = Default
                    , isMine = True
                    }
    in
    Array2D.set r c newCell grid


setMines : Int -> List ( Int, Int ) -> Array2D CellState -> Array2D CellState
setMines n coords grid =
    List.take n coords
        |> List.foldl (\( r, c ) acc -> setMine r c acc) grid


resetGrid : Array2D CellState -> Array2D CellState
resetGrid grid =
    Array2D.map (\_ -> defaultCellState) grid


revealCell : Array2D CellState -> Int -> Int -> CellState -> CellState
revealCell _ r c cell =
    { cell
        | revealed =
            if cell.isMine then
                RevealedMine

            else
                Revealed (modBy 9 (r + c))
    }


type Msg
    = Reset
    | SetBombs (List ( Int, Int ))
    | ClickRC Int Int
    | DebugRevealAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( resetGrid model, generate SetBombs (shuffle (allCoords model)) )

        SetBombs coords ->
            ( setMines 10 coords model, Cmd.none )

        ClickRC r c ->
            let
                maybeCell =
                    Array2D.get r c model
            in
            case maybeCell of
                Nothing ->
                    ( model, Cmd.none )

                Just cell ->
                    case cell.revealed of
                        Default ->
                            ( Array2D.set
                                r
                                c
                                (revealCell model r c cell)
                                model
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

        DebugRevealAll ->
            ( Array2D.indexedMap
                (\r c cell -> revealCell model r c cell)
                model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


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

        RevealedMine ->
            cellButtonView [ class "revealed" ] "💣"


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
