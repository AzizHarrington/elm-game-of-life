module Model exposing (..)


import Messages exposing (Msg)


import Array exposing (Array)
import Time exposing (millisecond)


type alias Model =
  { settings : Settings
  , grid : Grid
  }


type alias Settings =
  { running : Bool
  , timeStep : Float
  , highlight : Bool
  , gridCellWidth : Int
  , gridPixelWidth : Int
  , cellPixelWidth : Float
  }


type alias Grid =
  Array Cell


type alias Cell =
  { id : Int
  , isAlive : Bool
  }


init : ( Model, Cmd Msg )
init =
  let
    gridCellWidth =
      70

    gridPixelWidth =
      700

    initGrid =
      (Array.repeat (gridCellWidth ^ 2) False)
      |> Array.indexedMap Cell

    initSettings =
      { running = False
      , timeStep = 100 * millisecond
      , highlight = False
      , gridCellWidth = gridCellWidth
      , gridPixelWidth = gridPixelWidth
      , cellPixelWidth = toFloat gridPixelWidth / toFloat gridCellWidth
      }
  in
    ( { settings = initSettings, grid = initGrid }, Cmd.none )
