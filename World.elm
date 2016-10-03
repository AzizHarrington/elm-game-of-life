module World exposing (..)


import Array exposing (Array)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.App
import Html.Events exposing (onClick, onMouseDown, onMouseOver, onMouseUp)
import Html.Lazy exposing (lazy, lazy2)
import List
import Random
import Time exposing (Time, millisecond)


-- MODEL


type alias Model =
  { settings : Settings
  , grid : Grid
  }


type alias Settings =
  { running : Bool
  , timeStep : Float
  , highlight : Bool
  , gridCellWidth : Int
  , gridPixelWidth : Int
  , cellPixelWidth : Float
  }


type alias Grid =
  Array Cell


type alias Cell =
  { id : Int
  , isAlive : Bool
  }


init : ( Model, Cmd Msg )
init =
  let
    gridCellWidth =
      70

    gridPixelWidth =
      700

    initGrid =
      (Array.repeat (gridCellWidth ^ 2) False)
      |> Array.indexedMap Cell

    initSettings =
      { running = False
      , timeStep = 100 * millisecond
      , highlight = False
      , gridCellWidth = gridCellWidth
      , gridPixelWidth = gridPixelWidth
      , cellPixelWidth = toFloat gridPixelWidth / toFloat gridCellWidth
      }
  in
    ( { settings = initSettings, grid = initGrid }, Cmd.none )


-- MESSAGES


type Msg
  = ToggleCell Int
  | AgeWorld Time
  | ToggleTime
  | MouseDown
  | MouseUp
  | AnimateCell Int
  | RandomizeGrid
  | SetGrid (List Bool)
  | NoOp


-- VIEW

view : Model -> Html Msg
view model =
  div [ style [ ("-webkit-touch-callout", "none")
              , ("-webkit-user-select", "none")
              , ("-khtml-user-select", "none")
              , ("-moz-user-select", "none")
              , ("-ms-user-select", "none")
              , ("-o-user-select", "none")
              , ("user-select", "none")
              , ("margin-top", "100px")
              ]
      ]
      [ lazy controls model
      , worldGrid model
      ]


controls : Model -> Html Msg
controls model =
  let
    toggleText =
      if model.settings.running == True then "Pause Game" else "Start Game"

    cleanSlateGrid =
      List.repeat (model.settings.gridCellWidth ^ 2) False
  in
    div [ style [ ("margin", "0 auto")
                , ("margin-bottom", "25px")
                , ("width", (toString model.settings.gridPixelWidth) ++ "px")
                ]
        ]
        [ button [ onClick ToggleTime ] [ text toggleText ]
        , button [ onClick RandomizeGrid ] [ text "Randomize" ]
        , button [ onClick <| SetGrid cleanSlateGrid ] [ text "Reset" ]
        ]


worldGrid : Model -> Html Msg
worldGrid model =
  let
    lazyRender cell =
      lazy2 renderCell cell model.settings
  in
    div [ style [ ("width", (toString model.settings.gridPixelWidth) ++ "px")
                , ("height", (toString model.settings.gridPixelWidth) ++ "px")
                , ("outline", "1px solid")
                , ("margin", "0 auto")
                ]
        , onMouseDown MouseDown
        , onMouseUp MouseUp
        ]
        <| Array.toList (Array.map lazyRender model.grid)


renderCell : Cell -> Settings -> Html Msg
renderCell cell settings =
  let
    bgcolor =
      if cell.isAlive then
        "black"
      else
        "white"

    maybeAnimate id =
      if settings.highlight then AnimateCell id else NoOp
  in
    div [ style [ ("width", (toString settings.cellPixelWidth) ++ "px")
                , ("height", (toString settings.cellPixelWidth) ++ "px")
                , ("float", "left")
                , ("background-color", bgcolor)
                ]
        , onClick <| ToggleCell cell.id
        , onMouseOver <| maybeAnimate cell.id
        ]
        []


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    currentSettings = model.settings

    updateRunning state =
      { currentSettings | running = state }

    updateHighlight state =
      { currentSettings | highlight = state }
  in
    case msg of
      ToggleCell id ->
        ( { model | grid = Array.map (toggleCell id) model.grid }, Cmd.none )
      AgeWorld _ ->
        ( { model | grid = Array.map (age model) model.grid }, Cmd.none )
      ToggleTime ->
        ( { model | settings = updateRunning (not model.settings.running) }, Cmd.none )
      MouseDown ->
        ( { model | settings = updateHighlight True }, Cmd.none )
      MouseUp ->
        ( { model | settings = updateHighlight False }, Cmd.none )
      AnimateCell id ->
        ( { model | grid = Array.map (animateCell id) model.grid }, Cmd.none )
      RandomizeGrid ->
        ( { model | settings = updateRunning False }, Random.generate SetGrid (boolList model.settings) )
      SetGrid newStates ->
        let
          newGrid =
            newStates
            |> Array.fromList
            |> Array.indexedMap Cell
        in
          ( { model | grid = newGrid, settings = updateRunning False }, Cmd.none )
      NoOp ->
        ( model, Cmd.none )


boolList : Settings -> Random.Generator (List Bool)
boolList settings =
  Random.list (settings.gridCellWidth ^ 2) Random.bool


toggleCell : Int -> Cell -> Cell
toggleCell id cell =
  setCellState id cell (not cell.isAlive)


animateCell : Int -> Cell -> Cell
animateCell id cell =
  setCellState id cell True


setCellState : Int -> Cell -> Bool -> Cell
setCellState id cell newState =
  if cell.id == id then
    { cell | isAlive = newState }
  else
    cell


age : Model -> Cell -> Cell
age model cell =
  let
    numNeighbors =
      neighbors model cell.id
      |> List.filter (\c -> c.isAlive == True)
      |> List.foldl (\_ count -> count + 1) 0

    resurrect =
      cell.isAlive == False && numNeighbors == 3

    kill =
      cell.isAlive == True && (numNeighbors < 2 || numNeighbors > 3)

    newState =
      if resurrect then
        True
      else if kill then
        False
      else
        cell.isAlive
  in
    { cell | isAlive = newState }


neighbors : Model -> Int -> List Cell
neighbors model index =
  let
    gridCellWidth =
      model.settings.gridCellWidth

    perimeter =
      [ index + 1 , index - 1
      , index + gridCellWidth, index - gridCellWidth
      , index + gridCellWidth + 1, index - gridCellWidth + 1
      , index + gridCellWidth - 1, index - gridCellWidth - 1
      ]

    getCell position =
      if beyondWorldSide position then
        deadCell
      else
        case Array.get position model.grid of
          Just cell ->
            cell
          Nothing ->
            -- Dead cells beyond world grid top & bottom
            deadCell

    beyondWorldSide pos =
      abs ((rem index gridCellWidth) - (rem pos gridCellWidth)) > 1

    deadCell =
      Cell -1 False
  in
    List.map getCell perimeter


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.settings.running && not model.settings.highlight then
    Time.every model.settings.timeStep AgeWorld
  else
    Sub.none


-- MAIN


main : Program Never
main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
