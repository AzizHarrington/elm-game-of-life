# elm-game-of-life

[Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)
implemented in [Elm](https://elm-lang.org/). Cells evolve each tick by the
classic rules:

- A live cell with 2 or 3 live neighbors survives.
- A dead cell with exactly 3 live neighbors becomes alive.
- All other cells die or stay dead.

Neighbors are the 8 surrounding cells. The grid does not wrap — cells past the
edges are treated as dead.

The grid starts at 100×100, but its size is dynamic: loading a large preset
(the breeder) switches to a much bigger world. The world is drawn on a single
`<canvas>` rather than one DOM node per cell, so it scales to millions of cells.

## Running it

The project uses **Elm 0.19.1**. A local copy is installed under
`node_modules` (via `npm install`), so you don't need a global Elm.

Because rendering and mouse input go through JS ports, the app is **not** a
standalone HTML file — `index.html` is a checked-in wrapper that loads the
compiled `elm.js`. Build it, then open the page:

```sh
./node_modules/.bin/elm make Main.elm --output=elm.js
open index.html
```

`elm.js` is the build artifact and is gitignored; `index.html` is source.

To serve it over HTTP (any static server works), e.g. on port 5555:

```sh
npx http-server -p 5555     # then visit http://localhost:5555
```

## Controls

- **Start / Pause** — run or pause the simulation. It auto-pauses when no live
  cells remain.
- **Randomize** — fill the grid with a random pattern.
- **Reset** — clear the grid (back to the default 100×100 size).
- **Step** — advance a single generation (shown only while paused).
- **Zoom − / Zoom +** — scale the cells down/up; the grid scrolls within its
  viewport when it's bigger than the screen.
- **Slower / Faster** — halve/double the tick rate.
- **Click** a cell to toggle it alive/dead.
- **Click-and-drag** across cells to paint them alive.

Below the buttons, a readout shows **generation**, **live cells**, the current
**grid** size, **zoom** (pixels per cell), and **speed** (target generations per
second). Note that very large grids (the breeder) are limited by computation, so
the actual rate may fall short of the requested speed.

## Presets

Buttons that load a famous starting pattern, each showing off a different kind
of behavior. Loading one clears the grid and pauses, so hit **Start** to watch.

| Preset           | What it does                                                                 |
| ---------------- | --------------------------------------------------------------------------- |
| **Glider**       | The smallest spaceship — crawls diagonally across the grid.                 |
| **Glider Gun**   | A Gosper glider gun: stays put and fires a new glider every 30 generations. |
| **Pulsar**       | A large, symmetric period-3 oscillator.                                     |
| **Acorn**        | A 7-cell methuselah that erupts into long-running chaos.                    |
| **Puffer Train** | A spaceship that travels down the grid leaving a trail of debris.           |
| **Breeder**      | Gosper's classic breeder — the first pattern shown to grow *quadratically*. |

On this finite, non-wrapping grid the moving patterns eventually reach an edge
and their debris settles into still lifes — a random grid likewise decays into
stable "ash." That's expected Game of Life behavior, not a bug.

### About the Breeder

The Breeder is the heavyweight: Gosper's 1971 pattern (4060 cells in a 749-wide
bounding box) that sweeps right laying down glider guns, which then spray
gliders — so the live-cell count grows with the *square* of time. It needs room,
so loading it switches the world to **1200×1200** (1.44 million cells) and starts
it at the left edge. At that size the simulation runs roughly **1 generation per
second**, so it's best watched by letting it run for a while or stepping through
it. The pattern is parsed from the Golly pattern collection (`breeder.lif`).

## Project layout

The code follows the standard Elm Architecture (Model / Update / View),
split into focused modules:

| File                   | Responsibility                                              |
| ---------------------- | ----------------------------------------------------------- |
| `Main.elm`             | Wires everything together via `Browser.element`.            |
| `src/Model.elm`        | `Model` and `Settings` types, plus the initial state.       |
| `src/Messages.elm`     | The `Msg` type — every event the app can handle.            |
| `src/Update.elm`       | State transitions, including the Game of Life rules (`ageGrid`). |
| `src/View.elm`         | Renders the controls, counters, and the `<canvas>` element. |
| `src/Subscriptions.elm`| Clock tick that ages the world, plus inbound mouse ports.   |
| `src/Patterns.elm`     | The named presets and the logic for placing them on the grid. |
| `src/Ports.elm`        | Ports: `drawGrid` out to the canvas, mouse events back in.  |
| `src/Breeder.elm`      | The breeder's 4060 cells (generated from `breeder.lif`).    |
| `index.html`           | Wrapper that boots `elm.js`, draws the canvas, handles mouse. |

The grid is stored as an `Array Cell`, where each `Cell` knows its index `id`
and whether it `isAlive`. Cell position is derived from the index using the
grid width (e.g. neighbors are `id ± 1`, `id ± width`, etc.). Rendering is done
in JS: after every update Elm sends the live-cell indices over the `drawGrid`
port and `index.html` paints them onto the canvas.

Presets are defined in `Patterns.elm` as a list of `(row, col)` offsets, an
`Anchor` (`Center`, `Top`, or `Left`) saying where to drop them, and a
`gridSize` the world switches to when loaded. To add one, append a `Pattern` to
`all` — a button is generated for it automatically.
