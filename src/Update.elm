module Update exposing (..)


import Messages exposing (..)
import Model exposing (Model, Settings, Grid, Cell)


import Array
import Random


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    currentSettings =
      model.settings

    updateRunning state =
      { currentSettings | running = state }

    updateHighlight state =
      { currentSettings | highlight = state }
  in
    case msg of
      ToggleCell id ->
        let
          newGrid =
            toggleCell id model.grid
        in
          ( { model | grid = newGrid }, Cmd.none )
      AgeWorld _ ->
        ( { model | grid = Array.map (age model) model.grid }, Cmd.none )
      ToggleTime ->
        ( { model | settings = updateRunning (not model.settings.running) }, Cmd.none )
      MouseDown ->
        ( { model | settings = updateHighlight True }, Cmd.none )
      MouseUp ->
        ( { model | settings = updateHighlight False }, Cmd.none )
      AnimateCell id ->
        let
          newGrid =
            updateCellState id True model.grid
        in
          ( { model | grid = newGrid }, Cmd.none )
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


toggleCell : Int -> Grid -> Grid
toggleCell id grid =
  let
    state =
      case Array.get id grid of
        Just cell ->
          not cell.isAlive
        Nothing ->
          -- in the NOT LIKELY case cell lookup fails,
          -- just default to live when toggling
          True
  in
    updateCellState id state grid


updateCellState : Int -> Bool -> Grid -> Grid
updateCellState id state grid =
  Array.set id (Cell id state) grid


age : Model -> Cell -> Cell
age model cell =
  let
    numNeighbors =
      neighbors model cell.id
      |> List.filter (\c -> c.isAlive == True)
      |> List.foldl (\_ count -> count + 1) 0

    resurrect =
      numNeighbors == 3 && not cell.isAlive

    kill =
      (numNeighbors < 2 || numNeighbors > 3) && cell.isAlive

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
