module Model exposing (..)


import Messages exposing (Msg)
import Ports


type alias Model =
  { settings : Settings
  , generation : Int
  , population : Int
  , gravity : Int
  , reflect : Bool
  }


type alias Settings =
  { running : Bool
  , timeStep : Float
  , gridCellWidth : Int
  , cellPixelWidth : Float
  }


-- The grid size is dynamic: normal play uses a small grid, but loading a huge
-- pattern (the breeder) switches to a large one. Cell pixels shrink to keep the
-- canvas around 800px, bottoming out at 1px per cell for very large grids.
settingsForSize : Int -> Settings
settingsForSize size =
  { running = False
  , timeStep = 100
  , gridCellWidth = size
  , cellPixelWidth = Basics.max 1.0 (toFloat (800 // size))
  }


defaultSize : Int
defaultSize =
  100


init : () -> ( Model, Cmd Msg )
init _ =
  let
    settings =
      settingsForSize defaultSize
  in
    ( { settings = settings, generation = 0, population = 0, gravity = 0, reflect = False }
    , Ports.loadEngine
        { size = settings.gridCellWidth
        , cellPx = settings.cellPixelWidth
        , live = []
        }
    )
