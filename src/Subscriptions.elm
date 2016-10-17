module Subscriptions exposing (..)


import Messages exposing (..)
import Model exposing (Model, Settings, Grid, Cell)


import Array
import List
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
  if isRunning model then
    Time.every model.settings.timeStep AgeWorld
  else
    Sub.none


isRunning : Model -> Bool
isRunning model =
  model.settings.running && not model.settings.highlight
