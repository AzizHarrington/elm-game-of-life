module Update exposing (..)


import Messages exposing (..)
import Model exposing (..)


import Array
import Patterns
import Random


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleCell id ->
      let
        grid =
          toggleCell id model.grid
      in
        ( { model | grid = grid }, Cmd.none )

    Step ->
      let
        grid =
          ageGrid model
      in
        ( { model | grid = grid, generation = model.generation + 1 }, Cmd.none )

    AgeWorld _ ->
      let
        grid =
          ageGrid model

        hasLiveCells =
            grid
            |> Array.toList
            |> List.any (\c -> c.isAlive)

        settings =
          updateSettings model.settings (Running hasLiveCells)
      in
        ( { model | grid = grid, settings = settings, generation = model.generation + 1 }, Cmd.none )

    ToggleTime ->
      let
        settings =
          updateSettings model.settings (Running (not model.settings.running))
      in
        ( { model | settings = settings }, Cmd.none )

    MouseDown ->
      let
        settings =
          updateSettings model.settings (Highlight True)
      in
        ( { model | settings = settings }, Cmd.none )

    MouseUp ->
      let
        settings =
          updateSettings model.settings  (Highlight False)
      in
        ( { model | settings = settings }, Cmd.none )

    AnimateCell id ->
      let
        grid =
          updateCellState id True model.grid
      in
        ( { model | grid = grid }, Cmd.none )

    RandomizeGrid ->
      let
        settings =
          updateSettings model.settings (Running False)

        randomStates =
          Random.list (model.settings.gridCellWidth ^ 2) randomBool

        command =
          Random.generate SetGrid randomStates
      in
        ( { model | settings = settings }, command )

    SetGrid newStates ->
      let
        grid =
          newStates
          |> Array.fromList
          |> Array.indexedMap Cell

        settings =
          updateSettings model.settings (Running False)
      in
        ( { model | grid = grid, settings = settings, generation = 0 }, Cmd.none )

    LoadPattern pattern ->
      let
        deadGrid =
          Array.repeat (model.settings.gridCellWidth ^ 2) False
          |> Array.indexedMap Cell

        grid =
          Patterns.aliveIndices model.settings.gridCellWidth pattern
          |> List.foldl (\index g -> updateCellState index True g) deadGrid

        settings =
          updateSettings model.settings (Running False)
      in
        ( { model | grid = grid, settings = settings, generation = 0 }, Cmd.none )

    NoOp ->
      ( model, Cmd.none )


randomBool : Random.Generator Bool
randomBool =
  Random.uniform True [ False ]


updateSettings : Settings -> Setting -> Settings
updateSettings settings setting =
  case setting of
    Running state ->
      { settings | running = state }
    Highlight state ->
      { settings | highlight = state }


toggleCell : Int -> Grid -> Grid
toggleCell id grid =
  case Array.get id grid of
    Just cell ->
      updateCellState id (not cell.isAlive) grid
    Nothing ->
      -- in the NOT LIKELY case cell lookup fails,
      -- just default to live when toggling
      updateCellState id True grid


updateCellState : Int -> Bool -> Grid -> Grid
updateCellState id state grid =
  Array.set id (Cell id state) grid


ageGrid : Model -> Grid
ageGrid model =
  Array.map (ageCell model) model.grid


ageCell : Model -> Cell -> Cell
ageCell model cell =
  let
    numNeighbors =
      neighbors cell.id model
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


neighbors : Int -> Model -> List Cell
neighbors index model =
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
      abs ((remainderBy gridCellWidth index) - (remainderBy gridCellWidth pos)) > 1

    deadCell =
      Cell -1 False
  in
    List.map getCell perimeter
