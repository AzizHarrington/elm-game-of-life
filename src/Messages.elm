module Messages exposing (..)


import Time exposing (Time)


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
