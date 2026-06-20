port module Ports exposing
  ( loadEngine
  , randomizeEngine
  , stepEngine
  , setZoom
  , setGravity
  , setReflect
  , runEngine
  , population
  , tock
  )

-- The simulation lives in JS (a flat Uint8Array) for speed. Elm is the
-- controller: it owns the clock, generation count, and zoom/speed settings, and
-- drives the engine through these ports. See index.html for the engine itself.


-- Elm -> JS


-- Allocate a `size`×`size` world, set the given cell indices alive, and draw at
-- `cellPx` pixels per cell. Used for presets and clearing.
port loadEngine : { size : Int, cellPx : Float, live : List Int } -> Cmd msg


-- Allocate a `size`×`size` world filled with a random ~50% soup.
port randomizeEngine : { size : Int, cellPx : Float } -> Cmd msg


-- Advance the world one generation and redraw.
port stepEngine : () -> Cmd msg


-- Redraw the current world at a new pixel-per-cell size.
port setZoom : Float -> Cmd msg


-- Set the gravity strength (0 = off). Each generation, live cells drift toward
-- the local center of mass with probability proportional to this.
port setGravity : Int -> Cmd msg


-- Toggle reflecting walls. When true, out-of-bounds neighbors mirror the cells
-- just inside the edge instead of counting as dead.
port setReflect : Bool -> Cmd msg


-- Start/stop the background clock and set its interval (ms). The clock is a
-- Web Worker so it keeps ticking when the tab is hidden (unlike Time.every).
port runEngine : { running : Bool, ms : Float } -> Cmd msg


-- JS -> Elm


-- The live-cell count, reported after every change to the world.
port population : (Int -> msg) -> Sub msg


-- A clock tick from the worker; each one advances a generation.
port tock : (Int -> msg) -> Sub msg
