module Subscriptions exposing (..)


import Messages exposing (..)
import Model exposing (Model)
import Ports


import Time


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ tick model
    , Ports.cellToggled ToggleCell
    , Ports.cellPainted AnimateCell
    ]


tick : Model -> Sub Msg
tick model =
  if isRunning model then
    Time.every model.settings.timeStep AgeWorld
  else
    Sub.none


isRunning : Model -> Bool
isRunning model =
  model.settings.running && not model.settings.highlight
