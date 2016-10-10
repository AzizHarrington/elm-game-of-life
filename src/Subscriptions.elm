module Subscriptions exposing (..)


import Messages exposing (..)
import Model exposing (Model, Settings, Grid, Cell)


import Time


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.settings.running && not model.settings.highlight then
    Time.every model.settings.timeStep AgeWorld
  else
    Sub.none
