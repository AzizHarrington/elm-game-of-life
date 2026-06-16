module Messages exposing (..)


import Time

import Patterns exposing (Pattern)


type Msg
  = ToggleCell Int
  | Step
  | AgeWorld Time.Posix
  | ToggleTime
  | MouseDown
  | MouseUp
  | AnimateCell Int
  | RandomizeGrid
  | SetGrid (List Bool)
  | LoadPattern Pattern
  | NoOp
