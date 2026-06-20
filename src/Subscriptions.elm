module Subscriptions exposing (..)


import Messages exposing (..)
import Model exposing (Model)
import Ports


-- The clock lives in a Web Worker (so it survives backgrounded tabs); each tick
-- it sends advances a generation. Population updates come back the same way.
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Ports.tock (\_ -> Tick)
    , Ports.population PopulationChanged
    ]
