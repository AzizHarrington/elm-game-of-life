module Update exposing (..)


import Messages exposing (..)
import Model exposing (Model, Settings, Grid, Cell)


import Array
import Random


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
