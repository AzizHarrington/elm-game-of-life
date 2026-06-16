module Update exposing (..)


import Messages exposing (..)
import Model exposing (..)


import Array
import Patterns
import Ports
import Random


-- Every message produces a new model; we then scan the grid once for the live
-- cells, reuse that for both the population counter and the canvas, and push it
-- out the draw port.
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    ( m1, cmd ) =
      applyMsg msg model

    indices =
      liveIndices m1.grid

    population =
      List.length indices

    -- Nothing left alive? Stop the clock.
    settings =
      if population == 0 then
        updateSettings m1.settings (Running False)
      else
        m1.settings

    m2 =
      { m1 | population = population, settings = settings }
  in
    ( m2
    , Cmd.batch
        [ cmd
        , Ports.drawGrid
            { size = m2.settings.gridCellWidth
            , cellPx = m2.settings.cellPixelWidth
            , live = indices
            }
        ]
    )


applyMsg : Msg -> Model -> ( Model, Cmd Msg )
applyMsg msg model =
  case msg of
    ToggleCell id ->
      ( { model | grid = toggleCell id model.grid }, Cmd.none )

    Step ->
      ( { model | grid = ageGrid model, generation = model.generation + 1 }, Cmd.none )

    AgeWorld _ ->
      ( { model | grid = ageGrid model, generation = model.generation + 1 }, Cmd.none )

    ToggleTime ->
      ( { model | settings = updateSettings model.settings (Running (not model.settings.running)) }, Cmd.none )

    MouseDown ->
      ( { model | settings = updateSettings model.settings (Highlight True) }, Cmd.none )

    MouseUp ->
      ( { model | settings = updateSettings model.settings (Highlight False) }, Cmd.none )

    AnimateCell id ->
      ( { model | grid = updateCellState id True model.grid }, Cmd.none )

    RandomizeGrid ->
      let
        randomStates =
          Random.list (model.settings.gridCellWidth ^ 2) randomBool
      in
        ( { model | settings = updateSettings model.settings (Running False) }
        , Random.generate SetGrid randomStates
        )

    SetGrid newStates ->
      ( { model
          | grid = Array.fromList newStates
          , settings = updateSettings model.settings (Running False)
          , generation = 0
        }
      , Cmd.none
      )

    LoadPattern pattern ->
      let
        size =
          pattern.gridSize

        grid =
          Patterns.aliveIndices size pattern
            |> List.foldl (\index g -> updateCellState index True g) (emptyGrid size)
      in
        ( { model | grid = grid, settings = settingsForSize size, generation = 0 }, Cmd.none )

    ResetGrid ->
      ( { model | grid = emptyGrid defaultSize, settings = settingsForSize defaultSize, generation = 0 }, Cmd.none )

    ZoomIn ->
      ( { model | settings = setCellPx (Basics.min 40 (model.settings.cellPixelWidth * 2)) model.settings }, Cmd.none )

    ZoomOut ->
      ( { model | settings = setCellPx (Basics.max 1 (model.settings.cellPixelWidth / 2)) model.settings }, Cmd.none )

    SpeedUp ->
      ( { model | settings = setTimeStep (Basics.max 16 (model.settings.timeStep / 2)) model.settings }, Cmd.none )

    SlowDown ->
      ( { model | settings = setTimeStep (Basics.min 1000 (model.settings.timeStep * 2)) model.settings }, Cmd.none )

    NoOp ->
      ( model, Cmd.none )


-- Resize cells (zoom) and keep the canvas pixel width in sync.
setCellPx : Float -> Settings -> Settings
setCellPx px settings =
  { settings
    | cellPixelWidth = px
    , gridPixelWidth = round (toFloat settings.gridCellWidth * px)
  }


setTimeStep : Float -> Settings -> Settings
setTimeStep ms settings =
  { settings | timeStep = ms }


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
    Just alive ->
      Array.set id (not alive) grid
    Nothing ->
      grid


updateCellState : Int -> Bool -> Grid -> Grid
updateCellState id state grid =
  Array.set id state grid


-- Collect the indices of all live cells in a single left-to-right pass. Order
-- doesn't matter for drawing, so we don't bother reversing.
liveIndices : Grid -> List Int
liveIndices grid =
  Array.foldl
    (\alive ( i, acc ) -> ( i + 1, if alive then i :: acc else acc ))
    ( 0, [] )
    grid
    |> Tuple.second


ageGrid : Model -> Grid
ageGrid model =
  let
    grid =
      model.grid

    width =
      model.settings.gridCellWidth

    total =
      width * width
  in
    Array.indexedMap (\i alive -> nextState grid width total i alive) grid


nextState : Grid -> Int -> Int -> Int -> Bool -> Bool
nextState grid width total index alive =
  let
    n =
      liveNeighbors grid width total index
  in
    if alive then
      n == 2 || n == 3
    else
      n == 3


-- Count live neighbors without allocating any intermediate lists. The grid does
-- not wrap: a neighbor that would cross the left/right edge (column jump > 1) or
-- fall off the top/bottom (out of bounds) counts as dead.
liveNeighbors : Grid -> Int -> Int -> Int -> Int
liveNeighbors grid width total index =
  let
    col =
      remainderBy width index

    at pos =
      if pos < 0 || pos >= total then
        0
      else if abs (col - remainderBy width pos) > 1 then
        0
      else if aliveAt grid pos then
        1
      else
        0
  in
    at (index + 1)
      + at (index - 1)
      + at (index + width)
      + at (index - width)
      + at (index + width + 1)
      + at (index - width + 1)
      + at (index + width - 1)
      + at (index - width - 1)


aliveAt : Grid -> Int -> Bool
aliveAt grid pos =
  case Array.get pos grid of
    Just alive ->
      alive
    Nothing ->
      False
