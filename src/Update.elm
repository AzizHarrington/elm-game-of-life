module Update exposing (..)


import Messages exposing (..)
import Model exposing (..)


import Patterns
import Ports


-- After handling a message, if the run state or speed changed, tell the worker
-- clock to start/stop/re-pace. (Skipped for plain ticks, which don't change it.)
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    ( newModel, cmd ) =
      applyMsg msg model

    runChanged =
      newModel.settings.running /= model.settings.running
        || newModel.settings.timeStep /= model.settings.timeStep
  in
    if runChanged then
      ( newModel
      , Cmd.batch
          [ cmd
          , Ports.runEngine { running = newModel.settings.running, ms = newModel.settings.timeStep }
          ]
      )
    else
      ( newModel, cmd )


applyMsg : Msg -> Model -> ( Model, Cmd Msg )
applyMsg msg model =
  case msg of
    Step ->
      ( { model | generation = model.generation + 1 }, Ports.stepEngine () )

    Tick ->
      ( { model | generation = model.generation + 1 }, Ports.stepEngine () )

    ToggleTime ->
      ( { model | settings = setRunning (not model.settings.running) model.settings }, Cmd.none )

    RandomizeGrid ->
      ( { model | settings = setRunning False model.settings, generation = 0 }
      , Ports.randomizeEngine
          { size = model.settings.gridCellWidth
          , cellPx = model.settings.cellPixelWidth
          }
      )

    LoadPattern pattern ->
      let
        settings =
          settingsForSize pattern.gridSize
      in
        ( { model | settings = settings, generation = 0 }
        , Ports.loadEngine
            { size = settings.gridCellWidth
            , cellPx = settings.cellPixelWidth
            , live = Patterns.aliveIndices settings.gridCellWidth pattern
            }
        )

    ResetGrid ->
      let
        settings =
          settingsForSize defaultSize
      in
        ( { model | settings = settings, generation = 0 }
        , Ports.loadEngine
            { size = settings.gridCellWidth
            , cellPx = settings.cellPixelWidth
            , live = []
            }
        )

    ZoomIn ->
      zoomTo (Basics.min 40 (model.settings.cellPixelWidth * 2)) model

    ZoomOut ->
      zoomTo (Basics.max 1 (model.settings.cellPixelWidth / 2)) model

    SpeedUp ->
      ( { model | settings = setTimeStep (Basics.max 16 (model.settings.timeStep / 2)) model.settings }, Cmd.none )

    SlowDown ->
      ( { model | settings = setTimeStep (Basics.min 1000 (model.settings.timeStep * 2)) model.settings }, Cmd.none )

    GravityUp ->
      setGravity (Basics.min 8 (model.gravity + 1)) model

    GravityDown ->
      setGravity (Basics.max -8 (model.gravity - 1)) model

    ToggleReflect ->
      let
        reflect =
          not model.reflect
      in
        ( { model | reflect = reflect }, Ports.setReflect reflect )

    PopulationChanged n ->
      -- Nothing alive? Stop the clock.
      let
        settings =
          if n == 0 then
            setRunning False model.settings
          else
            model.settings
      in
        ( { model | population = n, settings = settings }, Cmd.none )

    NoOp ->
      ( model, Cmd.none )


zoomTo : Float -> Model -> ( Model, Cmd Msg )
zoomTo px model =
  ( { model | settings = setCellPx px model.settings }, Ports.setZoom px )


setGravity : Int -> Model -> ( Model, Cmd Msg )
setGravity g model =
  ( { model | gravity = g }, Ports.setGravity g )


setRunning : Bool -> Settings -> Settings
setRunning running settings =
  { settings | running = running }


setTimeStep : Float -> Settings -> Settings
setTimeStep ms settings =
  { settings | timeStep = ms }


setCellPx : Float -> Settings -> Settings
setCellPx px settings =
  { settings | cellPixelWidth = px }
