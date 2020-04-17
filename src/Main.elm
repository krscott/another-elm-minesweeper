module Main exposing (main)

import Array
import Array2D exposing (Array2D)
import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (generate)
import Shuffle exposing (shuffle)



-- DEFAULTS


defaultRows : Int
defaultRows =
    20


defaultCols : Int
defaultCols =
    20


defaultMines : Int
defaultMines =
    50



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
        , debugShowAll : Bool
        }
    , numMines : Int
    , rows : Int
    , cols : Int
    , grid : Array2D CellState
    , isGameOver : Bool
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
        , debugShowAll = False
        }
    , numMines = n
    , rows = rows
    , cols = cols
    , grid = Array2D.repeat rows cols defaultCellState
    , isGameOver = False
    }


gridToFlatList : Array2D a -> List a
gridToFlatList grid =
    Array.toList grid.data
        |> List.map (\arr -> Array.toList arr)
        |> List.concat


getRevealedCells : Array2D CellState -> Int
getRevealedCells grid =
    gridToFlatList grid
        |> List.map
            (\cell ->
                if cell.revealed == Default then
                    0

                else
                    1
            )
        |> List.sum


init : () -> ( Model, Cmd Msg )
init _ =
    update Reset (initModel defaultRows defaultCols defaultMines)



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


revealCell : Int -> Int -> CellState -> Array2D CellState -> CellState
revealCell r c cell grid =
    { cell
        | revealed =
            if cell.isMine then
                RevealedMine

            else
                let
                    neighborMines =
                        [ ( r - 1, c - 1 )
                        , ( r - 1, c )
                        , ( r - 1, c + 1 )
                        , ( r, c - 1 )
                        , ( r, c + 1 )
                        , ( r + 1, c - 1 )
                        , ( r + 1, c )
                        , ( r + 1, c + 1 )
                        ]
                            |> List.filterMap (\( nr, nc ) -> Array2D.get nr nc grid)
                            |> List.map
                                (\neighborCell ->
                                    if neighborCell.isMine then
                                        1

                                    else
                                        0
                                )
                            |> List.sum
                in
                Revealed neighborMines
    }


revealCellsFrom : Int -> Int -> Array2D CellState -> Array2D CellState
revealCellsFrom r c grid =
    case Array2D.get r c grid of
        Nothing ->
            grid

        Just oldCell ->
            let
                newCell =
                    revealCell r c oldCell grid

                newGrid =
                    Array2D.set r c newCell grid
            in
            case newCell.revealed of
                Revealed 0 ->
                    [ ( r - 1, c - 1 )
                    , ( r - 1, c )
                    , ( r - 1, c + 1 )
                    , ( r, c - 1 )
                    , ( r, c + 1 )
                    , ( r + 1, c - 1 )
                    , ( r + 1, c )
                    , ( r + 1, c + 1 )
                    ]
                        |> List.filterMap
                            (\( nr, nc ) ->
                                Array2D.get nr nc newGrid
                                    |> Maybe.andThen (\cell -> Just ( nr, nc, cell ))
                            )
                        |> List.filter (\( _, _, cell ) -> cell.revealed == Default)
                        |> List.foldl
                            (\( nr, nc, _ ) acc -> revealCellsFrom nr nc acc)
                            newGrid

                _ ->
                    newGrid


type alias FirstMoveRecord =
    { r : Int
    , c : Int
    , coords : List ( Int, Int )
    }


type Msg
    = Reset
    | ChangeInput UserInput String
    | FirstMove FirstMoveRecord
    | ClickRC Int Int
    | DebugRevealAll
    | DebugToggleShowAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        genNewMines =
            \r c grid ->
                generate FirstMove
                    (Random.map
                        (\coords ->
                            { r = r
                            , c = c
                            , coords = coords
                            }
                        )
                        (shuffle
                            (allCoords grid)
                        )
                    )

        digCell =
            \r c model_ ->
                let
                    newGrid =
                        revealCellsFrom r c model_.grid

                    clickedCell =
                        Array2D.get r c newGrid |> Maybe.map (\cell -> cell.isMine)

                    isGameOver =
                        clickedCell == Just True
                in
                ( { model_
                    | grid = newGrid
                    , isGameOver = isGameOver
                  }
                , Cmd.none
                )
    in
    case msg of
        Reset ->
            let
                newModel =
                    initModel model.rows model.cols model.numMines
            in
            ( newModel, Cmd.none )

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

        DebugToggleShowAll ->
            let
                userInputs =
                    model.userInputs
            in
            ( { model
                | userInputs =
                    { userInputs
                        | debugShowAll = not userInputs.debugShowAll
                    }
              }
            , Cmd.none
            )

        FirstMove { r, c, coords } ->
            let
                grid =
                    setMines model.numMines coords model.grid
            in
            case Array2D.get r c grid of
                Nothing ->
                    ( model, Cmd.none )

                Just cell ->
                    let
                        safeGrid =
                            if cell.isMine && model.numMines < List.length coords then
                                -- Add an extra mine, then remove the clicked one
                                grid
                                    |> setMines (model.numMines + 1) coords
                                    |> Array2D.set r c defaultCellState

                            else
                                grid
                    in
                    { model | grid = safeGrid }
                        |> digCell r c

        ClickRC r c ->
            if getRevealedCells model.grid == 0 then
                -- Generate mines on first move
                ( model, genNewMines r c model.grid )

            else
                digCell r c model

        DebugRevealAll ->
            ( { model
                | grid =
                    Array2D.indexedMap
                        (\r c cell -> revealCell r c cell model.grid)
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


cellView : Bool -> Int -> Int -> CellState -> Html Msg
cellView showMines row col cell =
    case cell.revealed of
        Default ->
            cellButtonView
                (if showMines then
                    []

                 else
                    [ onClick (ClickRC row col) ]
                )
                (if showMines && cell.isMine then
                    "💣"

                 else
                    ""
                )

        Revealed 0 ->
            cellButtonView [ class "revealed" ] ""

        Revealed n ->
            cellButtonView
                [ class (String.append "revealed revealed-" (String.fromInt n))
                ]
                (String.fromInt n)

        RevealedMine ->
            cellButtonView [ class "revealed mine" ] "💥"


uiView : UserInput -> String -> String -> Html Msg
uiView uiType name uiValue =
    div [ class "user-input" ]
        [ div [] [ text name ]
        , input
            [ type_ "number", value uiValue, onInput (ChangeInput uiType) ]
            []
        ]


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name uiValue =
    label
        []
        [ input [ type_ "checkbox", onClick msg, checked uiValue ] []
        , text name
        ]


view : Model -> Html Msg
view model =
    div []
        [ div
            (if model.isGameOver then
                [ class "gameboard gameover" ]

             else
                [ class "gameboard" ]
            )
            (List.indexedMap
                (\r row ->
                    div []
                        (List.indexedMap
                            (\c cell ->
                                cellView
                                    (model.userInputs.debugShowAll
                                        || model.isGameOver
                                    )
                                    r
                                    c
                                    cell
                            )
                            (Array.toList row)
                        )
                )
                (Array.toList model.grid.data)
            )
        , div
            [ class "menu" ]
            [ button [ onClick Reset ] [ text "Reset" ]
            , uiView NumMines "Mines" model.userInputs.numMines
            , uiView Rows "Rows" model.userInputs.rows
            , uiView Cols "Cols" model.userInputs.cols
            , div []
                [ checkbox
                    DebugToggleShowAll
                    "DEBUG: Show Bombs"
                    model.userInputs.debugShowAll
                ]
            , button [ onClick DebugRevealAll ] [ text "DEBUG: Reveal All" ]
            ]
        ]
