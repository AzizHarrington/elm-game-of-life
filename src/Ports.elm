port module Ports exposing (drawGrid, cellToggled, cellPainted)

-- Rendering and pointer input go through a <canvas> in JS rather than one DOM
-- node per cell, so the grid can scale to millions of cells (e.g. the breeder).


-- Outbound: hand the current frame to the canvas. `size` is the grid width in
-- cells, `cellPx` the pixel size of each cell, `live` the indices to fill.
port drawGrid : { size : Int, cellPx : Float, live : List Int } -> Cmd msg


-- Inbound: a single click toggles a cell; dragging paints cells alive.
port cellToggled : (Int -> msg) -> Sub msg


port cellPainted : (Int -> msg) -> Sub msg
