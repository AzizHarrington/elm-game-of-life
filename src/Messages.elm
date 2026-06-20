module Messages exposing (..)


import Patterns exposing (Pattern)


type Msg
  = Step
  | Tick
  | ToggleTime
  | RandomizeGrid
  | LoadPattern Pattern
  | ResetGrid
  | ZoomIn
  | ZoomOut
  | SpeedUp
  | SlowDown
  | GravityUp
  | GravityDown
  | ToggleReflect
  | PopulationChanged Int
  | NoOp
