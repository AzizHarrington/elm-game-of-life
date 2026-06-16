module Patterns exposing (Anchor(..), Pattern, all, aliveIndices, puffer)

import Breeder


type Anchor
  = Center
  | Top
  | Left


type alias Pattern =
  { name : String
  , anchor : Anchor -- where on the grid to drop the pattern
  , gridSize : Int -- grid width (in cells) to switch to when this pattern loads
  , cells : List ( Int, Int ) -- (row, col) offsets within the pattern's bounding box
  }


all : List Pattern
all =
  [ glider
  , gosperGun
  , pulsar
  , acorn
  , puffer
  , breeder
  ]


-- Given the grid width, return the flat indices that should be alive when the
-- pattern is placed on the grid according to its anchor.
aliveIndices : Int -> Pattern -> List Int
aliveIndices gridWidth pattern =
  let
    height =
      pattern.cells
        |> List.map Tuple.first
        |> List.maximum
        |> Maybe.withDefault 0
        |> (+) 1

    width =
      pattern.cells
        |> List.map Tuple.second
        |> List.maximum
        |> Maybe.withDefault 0
        |> (+) 1

    rowOffset =
      case pattern.anchor of
        Center ->
          (gridWidth - height) // 2

        Top ->
          2

        Left ->
          (gridWidth - height) // 2

    colOffset =
      case pattern.anchor of
        Left ->
          20

        _ ->
          (gridWidth - width) // 2

    toIndex ( row, col ) =
      (rowOffset + row) * gridWidth + (colOffset + col)
  in
    List.map toIndex pattern.cells


glider : Pattern
glider =
  { name = "Glider"
  , anchor = Center
  , gridSize = 100
  , cells =
      [ ( 0, 1 )
      , ( 1, 2 )
      , ( 2, 0 ), ( 2, 1 ), ( 2, 2 )
      ]
  }


acorn : Pattern
acorn =
  { name = "Acorn"
  , anchor = Center
  , gridSize = 100
  , cells =
      [ ( 0, 1 )
      , ( 1, 3 )
      , ( 2, 0 ), ( 2, 1 ), ( 2, 4 ), ( 2, 5 ), ( 2, 6 )
      ]
  }


-- Period-3 oscillator. Built from four horizontal and four vertical bars so we
-- don't have to spell out all 48 cells by hand.
pulsar : Pattern
pulsar =
  let
    arm =
      [ 2, 3, 4, 8, 9, 10 ]

    spine =
      [ 0, 5, 7, 12 ]

    horizontal =
      List.concatMap (\row -> List.map (\col -> ( row, col )) arm) spine

    vertical =
      List.concatMap (\col -> List.map (\row -> ( row, col )) arm) spine
  in
    { name = "Pulsar"
    , anchor = Center
    , gridSize = 100
    , cells = horizontal ++ vertical
    }


-- Emits a new glider every 30 generations, forever.
gosperGun : Pattern
gosperGun =
  { name = "Glider Gun"
  , anchor = Center
  , gridSize = 100
  , cells =
      [ ( 4, 0 ), ( 5, 0 ), ( 4, 1 ), ( 5, 1 )
      , ( 4, 10 ), ( 5, 10 ), ( 6, 10 )
      , ( 3, 11 ), ( 7, 11 )
      , ( 2, 12 ), ( 8, 12 )
      , ( 2, 13 ), ( 8, 13 )
      , ( 5, 14 )
      , ( 3, 15 ), ( 7, 15 )
      , ( 4, 16 ), ( 5, 16 ), ( 6, 16 )
      , ( 5, 17 )
      , ( 2, 20 ), ( 3, 20 ), ( 4, 20 )
      , ( 2, 21 ), ( 3, 21 ), ( 4, 21 )
      , ( 1, 22 ), ( 5, 22 )
      , ( 0, 24 ), ( 1, 24 ), ( 5, 24 ), ( 6, 24 )
      , ( 2, 34 ), ( 3, 34 ), ( 2, 35 ), ( 3, 35 )
      ]
  }


-- A single puffer train: a B-heptomino escorted by two lightweight spaceships.
-- It crawls forward at c/2 leaving a trail of debris behind it. (Alan Hensel's
-- puffer train.) As written it travels "up" (toward row 0).
pufferCells : List ( Int, Int )
pufferCells =
  [ ( 0, 3 )
  , ( 1, 4 )
  , ( 2, 0 ), ( 2, 4 )
  , ( 3, 1 ), ( 3, 2 ), ( 3, 3 ), ( 3, 4 )
  , ( 7, 0 )
  , ( 8, 1 ), ( 8, 2 )
  , ( 9, 2 )
  , ( 10, 2 )
  , ( 11, 1 )
  , ( 14, 3 )
  , ( 15, 4 )
  , ( 16, 0 ), ( 16, 4 )
  , ( 17, 1 ), ( 17, 2 ), ( 17, 3 ), ( 17, 4 )
  ]


-- Rotate a list of cells 90° clockwise. Life is rotation-symmetric, so the
-- behavior is preserved -- this just turns the puffer's "up" travel into "right".
rotateCW : List ( Int, Int ) -> List ( Int, Int )
rotateCW cells =
  let
    maxRow =
      cells |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 0
  in
    List.map (\( row, col ) -> ( col, maxRow - row )) cells


-- A single puffer train. It travels downward, so we start it at the top of the
-- grid to give it the full height as runway, leaving a trail of debris behind.
puffer : Pattern
puffer =
  { name = "Puffer Train"
  , anchor = Top
  , gridSize = 100
  , cells = rotateCW pufferCells
  }


-- Gosper's breeder: the first pattern shown to grow quadratically. It sweeps
-- right laying down glider guns, which in turn spray gliders. It is huge (4060
-- cells, 749 wide) and needs a correspondingly huge grid, so loading it
-- switches to a 1200-cell-wide world and starts it at the left edge.
breeder : Pattern
breeder =
  { name = "Breeder"
  , anchor = Left
  , gridSize = 1200
  , cells = Breeder.cells
  }
