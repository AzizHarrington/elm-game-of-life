module Model exposing (..)


import Messages exposing (Msg)
import Ports


import Array exposing (Array)


type alias Model =
  { settings : Settings
  , grid : Grid
  , generation : Int
  , population : Int
  }


type alias Settings =
  { running : Bool
  , timeStep : Float
  , highlight : Bool
  , gridCellWidth : Int
  , gridPixelWidth : Int
  , cellPixelWidth : Float
  }


type Setting
  = Running Bool
  | Highlight Bool


-- The grid is a flat array of booleans (alive/dead); a cell's position is its
-- index. Storing plain Bools (rather than a record per cell) keeps the hot
-- ageing loop allocation-free, which matters a lot on the huge breeder grid.
type alias Grid =
  Array Bool


-- The grid size is dynamic: normal play uses a small grid, but loading a huge
-- pattern (the breeder) switches to a large one. Cell pixels shrink to keep the
-- canvas around 800px, bottoming out at 1px per cell for very large grids.
settingsForSize : Int -> Settings
settingsForSize size =
  let
    cellPx =
      Basics.max 1.0 (toFloat (800 // size))
  in
    { running = False
    , timeStep = 100
    , highlight = False
    , gridCellWidth = size
    , gridPixelWidth = round (toFloat size * cellPx)
    , cellPixelWidth = cellPx
    }


emptyGrid : Int -> Grid
emptyGrid size =
  Array.repeat (size * size) False


defaultSize : Int
defaultSize =
  100


init : () -> ( Model, Cmd Msg )
init _ =
  let
    settings =
      settingsForSize defaultSize

    model =
      { settings = settings
      , grid = emptyGrid defaultSize
      , generation = 0
      , population = 0
      }
  in
    ( model
    , Ports.drawGrid
        { size = settings.gridCellWidth
        , cellPx = settings.cellPixelWidth
        , live = []
        }
    )
