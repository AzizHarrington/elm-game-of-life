module Messages exposing (..)


import Time exposing (Time)


type Msg
  = ToggleCell Int
  | Step
  | AgeWorld Time
  | ToggleTime
  | MouseDown
  | MouseUp
  | AnimateCell Int
  | RandomizeGrid
  | SetGrid (List Bool)
  | NoOp
