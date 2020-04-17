module Main exposing (main)

-- import Random.List exposing (shuffle)

import Array
import Array2D exposing (Array2D)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
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
    { userInputs :
        { numMines : String
        , rows : String
        , cols : String
        }
    , numMines : Int
    , rows : Int
    , cols : Int
    , grid : Array2D CellState
    }


type UserInput
    = NumMines
    | Rows
    | Cols


initModel : Int -> Int -> Int -> Model
initModel rows cols n =
    { userInputs =
        { numMines = String.fromInt n
        , rows = String.fromInt rows
        , cols = String.fromInt cols
        }
    , numMines = n
    , rows = rows
    , cols = cols
    , grid = Array2D.initialize rows cols (\_ _ -> defaultCellState)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    update Reset (initModel 10 15 20)



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
    | ChangeInput UserInput String
    | SetBombs (List ( Int, Int ))
    | ClickRC Int Int
    | DebugRevealAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            let
                newModel =
                    initModel model.rows model.cols model.numMines
            in
            ( newModel
            , generate SetBombs (shuffle (allCoords newModel.grid))
            )

        ChangeInput k v ->
            let
                vPosIntOr =
                    \def ->
                        case String.toInt v of
                            Nothing ->
                                def

                            Just x ->
                                if x > 0 then
                                    x

                                else
                                    def

                userInputs =
                    model.userInputs

                newModel =
                    case k of
                        NumMines ->
                            { model
                                | numMines = vPosIntOr model.numMines
                                , userInputs = { userInputs | numMines = v }
                            }

                        Rows ->
                            { model
                                | rows = vPosIntOr model.rows
                                , userInputs = { userInputs | rows = v }
                            }

                        Cols ->
                            { model
                                | cols = vPosIntOr model.cols
                                , userInputs = { userInputs | cols = v }
                            }
            in
            ( newModel, Cmd.none )

        SetBombs coords ->
            ( { model | grid = setMines model.numMines coords model.grid }
            , Cmd.none
            )

        ClickRC r c ->
            let
                maybeCell =
                    Array2D.get r c model.grid
            in
            case maybeCell of
                Nothing ->
                    ( model, Cmd.none )

                Just cell ->
                    case cell.revealed of
                        Default ->
                            ( { model
                                | grid =
                                    Array2D.set
                                        r
                                        c
                                        (revealCell model.grid r c cell)
                                        model.grid
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

        DebugRevealAll ->
            ( { model
                | grid =
                    Array2D.indexedMap
                        (\r c cell -> revealCell model.grid r c cell)
                        model.grid
              }
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


uiView : String -> UserInput -> String -> Html Msg
uiView label uiType uiValue =
    div [ class "user-input" ]
        [ div [] [ text label ]
        , input
            [ type_ "number", value uiValue, onInput (ChangeInput uiType) ]
            []
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "gameboard" ]
            (List.indexedMap
                (\r row ->
                    div []
                        (List.indexedMap
                            (\c cell -> cellView r c cell)
                            (Array.toList row)
                        )
                )
                (Array.toList model.grid.data)
            )
        , div
            [ class "menu" ]
            [ button [ onClick Reset ] [ text "Reset" ]
            , button [ onClick DebugRevealAll ] [ text "DEBUG: Reveal All" ]
            , uiView "Mines" NumMines model.userInputs.numMines
            , uiView "Rows" Rows model.userInputs.rows
            , uiView "Cols" Cols model.userInputs.cols
            ]
        ]
