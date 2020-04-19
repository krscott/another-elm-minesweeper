module Main exposing (main)

import Array
import Array2D exposing (Array2D)
import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (custom, onClick, onInput)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode
import Random exposing (generate)
import Shuffle exposing (shuffle)
import Time



-- DEFAULTS


defaultRows : Int
defaultRows =
    9


defaultCols : Int
defaultCols =
    9


defaultMines : Int
defaultMines =
    10



-- CONSTANTS


emojiMine : String
emojiMine =
    "💣"


emojiFlag : String
emojiFlag =
    "🚩"


emojiFlagWrong : String
emojiFlagWrong =
    "✖️"


emojiBoom : String
emojiBoom =
    "💥"


emojiSmiley : String
emojiSmiley =
    "🙂"


emojiSmileyO : String
emojiSmileyO =
    "😮"


emojiSmileyDead : String
emojiSmileyDead =
    "😵"


emojiSmileyWin : String
emojiSmileyWin =
    "😎"



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


type UserInput
    = NumMines
    | Rows
    | Cols


type CellRevealedState
    = Default
    | Flagged
    | Revealed Int
    | RevealedMine


type alias CellState =
    { revealed : CellRevealedState
    , isMine : Bool
    }


defaultCellState : CellState
defaultCellState =
    { revealed = Default, isMine = False }


type GamePhase
    = Idle
    | Playing
    | Won
    | Lost


type MenuType
    = None
    | Options
    | About


type alias Model =
    { userInputs :
        { menuOpen : MenuType
        , numMines : String
        , rows : String
        , cols : String
        , debugShowAll : Bool
        , leftButtonDown : Bool
        }
    , timeStart : Time.Posix
    , timeCurrent : Time.Posix
    , numMines : Int
    , rows : Int
    , cols : Int
    , grid : Array2D CellState
    , gamePhase : GamePhase
    }


initModel : Int -> Int -> Int -> Model
initModel rows cols n =
    { userInputs =
        { menuOpen = None
        , numMines = String.fromInt n
        , rows = String.fromInt rows
        , cols = String.fromInt cols
        , debugShowAll = False
        , leftButtonDown = False
        }
    , timeStart = Time.millisToPosix 0
    , timeCurrent = Time.millisToPosix 0
    , numMines = n
    , rows = rows
    , cols = cols
    , grid = Array2D.repeat rows cols defaultCellState
    , gamePhase = Idle
    }


init : () -> ( Model, Cmd Msg )
init _ =
    update Reset (initModel defaultRows defaultCols defaultMines)



-- UPDATE


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
                case cell.revealed of
                    Default ->
                        0

                    Flagged ->
                        0

                    Revealed _ ->
                        1

                    RevealedMine ->
                        1
            )
        |> List.sum


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
    | Tick Time.Posix
    | ChangeInput UserInput String
    | FirstMove FirstMoveRecord
    | ClickRC Int Int
    | RightClickRC Int Int
    | DebugRevealAll
    | DebugToggleShowAll
    | LeftMouseDown Bool
    | OpenMenu MenuType
    | Nop


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

                    maybeClickedCellIsMine =
                        Array2D.get r c newGrid |> Maybe.map (\cell -> cell.isMine)

                    isGameLost =
                        maybeClickedCellIsMine == Just True

                    isGameWon =
                        (gridToFlatList newGrid
                            |> List.map
                                (\cell ->
                                    case ( cell.isMine, cell.revealed ) of
                                        ( False, Default ) ->
                                            1

                                        ( False, Flagged ) ->
                                            1

                                        _ ->
                                            0
                                )
                            |> List.sum
                        )
                            == 0
                in
                ( { model_
                    | grid = newGrid
                    , gamePhase =
                        if isGameLost then
                            Lost

                        else if isGameWon then
                            Won

                        else
                            Playing
                  }
                , Cmd.none
                )
    in
    case msg of
        Reset ->
            let
                posIntOr =
                    \str def ->
                        case String.toInt str of
                            Nothing ->
                                def

                            Just x ->
                                if x > 0 then
                                    x

                                else
                                    def

                newModel =
                    initModel
                        (posIntOr model.userInputs.rows model.rows)
                        (posIntOr model.userInputs.cols model.cols)
                        (posIntOr model.userInputs.numMines model.numMines)
            in
            ( newModel, Cmd.none )

        Tick time ->
            if model.gamePhase == Playing then
                let
                    timeStart =
                        if 0 == Time.posixToMillis model.timeStart then
                            time

                        else
                            model.timeStart
                in
                ( { model
                    | timeStart = timeStart
                    , timeCurrent = time
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        OpenMenu menuType ->
            let
                userInputs =
                    model.userInputs

                newMenuOpen =
                    if userInputs.menuOpen == menuType then
                        -- toggle off if already open
                        None

                    else
                        menuType
            in
            ( { model
                | userInputs =
                    { userInputs
                        | menuOpen = newMenuOpen
                    }
              }
            , Cmd.none
            )

        ChangeInput k v ->
            let
                userInputs =
                    model.userInputs

                newModel =
                    case k of
                        NumMines ->
                            { model
                                | userInputs = { userInputs | numMines = v }
                            }

                        Rows ->
                            { model
                                | userInputs = { userInputs | rows = v }
                            }

                        Cols ->
                            { model
                                | userInputs = { userInputs | cols = v }
                            }
            in
            ( newModel, Cmd.none )

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

        RightClickRC r c ->
            let
                grid =
                    model.grid

                newGrid =
                    case Array2D.get r c grid of
                        Nothing ->
                            grid

                        Just cell ->
                            case cell.revealed of
                                Default ->
                                    Array2D.set r c { cell | revealed = Flagged } grid

                                Flagged ->
                                    Array2D.set r c { cell | revealed = Default } grid

                                _ ->
                                    grid
            in
            ( { model | grid = newGrid }, Cmd.none )

        LeftMouseDown isDown ->
            let
                userInputs =
                    model.userInputs
            in
            ( { model
                | userInputs = { userInputs | leftButtonDown = isDown }
              }
            , Cmd.none
            )

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

        DebugRevealAll ->
            ( { model
                | grid =
                    Array2D.indexedMap
                        (\r c cell -> revealCell r c cell model.grid)
                        model.grid
                , gamePhase = Lost
              }
            , Cmd.none
            )

        Nop ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick



-- VIEW


onRightClick : msg -> Attribute msg
onRightClick msg =
    custom "contextmenu"
        (Json.Decode.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )


cellButtonView : List (Attribute msg) -> String -> Html msg
cellButtonView attrs str =
    span (class "gamecell" :: attrs) [ p [] [ text str ] ]


cellView : Bool -> GamePhase -> Int -> Int -> CellState -> Html Msg
cellView debugShow gamePhase row col cell =
    let
        gameOver =
            gamePhase == Won || gamePhase == Lost

        showMines =
            debugShow || gameOver
    in
    case cell.revealed of
        Default ->
            cellButtonView
                (if gameOver then
                    []

                 else
                    [ onClick (ClickRC row col)
                    , onRightClick (RightClickRC row col)
                    ]
                )
                (case ( cell.isMine, gamePhase, debugShow ) of
                    ( True, _, True ) ->
                        emojiMine

                    ( True, Lost, _ ) ->
                        emojiMine

                    ( True, Won, _ ) ->
                        emojiFlag

                    _ ->
                        ""
                )

        Flagged ->
            cellButtonView
                (class "flagged"
                    :: (if gameOver then
                            []

                        else
                            [ onRightClick (RightClickRC row col)
                            ]
                       )
                )
                (if showMines && not cell.isMine then
                    emojiFlagWrong

                 else
                    emojiFlag
                )

        Revealed 0 ->
            cellButtonView [ class "revealed" ] ""

        Revealed n ->
            cellButtonView
                [ class (String.append "revealed revealed-" (String.fromInt n))
                ]
                (String.fromInt n)

        RevealedMine ->
            cellButtonView [ class "revealed mine" ] emojiBoom


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
    let
        apparentRemaining =
            if model.gamePhase == Won then
                0

            else
                min 999 model.numMines
                    - (gridToFlatList model.grid
                        |> List.filter (\cell -> cell.revealed == Flagged)
                        |> List.length
                      )

        duration =
            min 999
                (round
                    (toFloat
                        (Time.posixToMillis model.timeCurrent
                            - Time.posixToMillis model.timeStart
                        )
                        / 1000.0
                    )
                )

        isGameOver =
            model.gamePhase == Won || model.gamePhase == Lost

        smiley =
            case ( model.gamePhase, model.userInputs.leftButtonDown ) of
                ( Won, _ ) ->
                    emojiSmileyWin

                ( Lost, _ ) ->
                    emojiSmileyDead

                ( Playing, True ) ->
                    emojiSmileyO

                ( Idle, True ) ->
                    emojiSmileyO

                ( Playing, False ) ->
                    emojiSmiley

                ( Idle, False ) ->
                    emojiSmiley

        menubarItem =
            \menuType name ->
                div
                    ((if model.userInputs.menuOpen == menuType then
                        [ class "highlight" ]

                      else
                        []
                     )
                        ++ [ onClick (OpenMenu menuType) ]
                    )
                    [ text name ]

        menubar =
            div [ class "menubar" ]
                [ menubarItem Options "Options"
                , menubarItem About "About"
                ]

        optionsMenu =
            div
                [ class "menu" ]
                [ uiView NumMines "Mines" model.userInputs.numMines
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

        header =
            div [ class "header" ]
                [ div [ class "remaining-count" ]
                    [ text (String.padLeft 3 '0' (String.fromInt apparentRemaining)) ]
                , div
                    [ class "reset-button", onClick Reset ]
                    [ p [] [ text smiley ] ]
                , div [ class "time" ]
                    [ text (String.padLeft 3 '0' (String.fromInt duration)) ]
                ]

        gamecells =
            div
                [ Mouse.onDown
                    (\event ->
                        if event.button == Mouse.MainButton then
                            LeftMouseDown True

                        else
                            Nop
                    )
                , Mouse.onUp
                    (\event ->
                        if event.button == Mouse.MainButton then
                            LeftMouseDown False

                        else
                            Nop
                    )
                , class "gamecells"
                ]
                (List.indexedMap
                    (\r row ->
                        div []
                            (List.indexedMap
                                (\c cell ->
                                    cellView
                                        model.userInputs.debugShowAll
                                        model.gamePhase
                                        r
                                        c
                                        cell
                                )
                                (Array.toList row)
                            )
                    )
                    (Array.toList model.grid.data)
                )

        gameboard =
            div
                (class "gameboard"
                    :: (if isGameOver then
                            [ class "gameover" ]

                        else
                            []
                       )
                    ++ (if model.userInputs.leftButtonDown then
                            [ class "left-mouse-down" ]

                        else
                            []
                       )
                )
                ((case model.userInputs.menuOpen of
                    None ->
                        []

                    Options ->
                        [ optionsMenu ]

                    About ->
                        [ optionsMenu ]
                 )
                    ++ [ gamecells ]
                )
    in
    div [ onRightClick Nop ]
        [ menubar
        , main_ []
            [ header
            , gameboard
            ]
        ]
