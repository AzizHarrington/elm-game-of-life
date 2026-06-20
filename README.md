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
(the breeder) switches to a much bigger world. To make grids of millions of
cells fast, the simulation and rendering run in **JavaScript** over a flat
`Uint8Array`, drawn on a single `<canvas>`. Elm stays as the controller — it
owns the UI, the clock, the generation count, and the zoom/speed settings, and
drives the engine through ports (see [Architecture](#architecture)).

## Running it

The project uses **Elm 0.19.1**. A local copy is installed under
`node_modules` (via `npm install`), so you don't need a global Elm.

Because the engine and rendering live in JS, the app is **not** a standalone
HTML file — `index.html` is a checked-in wrapper that loads the compiled
`elm.js` and contains the JS engine. The simplest way to build and serve:

```sh
npm start          # builds elm.js, serves at http://localhost:5555/
```

Or build and open manually:

```sh
npm run build      # elm make Main.elm --output=elm.js --optimize
open index.html
```

`elm.js` is the build artifact and is gitignored; `index.html` is source.

## Controls

- **Start / Pause** — run or pause the simulation. It auto-pauses when no live
  cells remain.
- **Randomize** — fill the grid with a random pattern.
- **Reset** — clear the grid (back to the default 100×100 size).
- **Step** — advance a single generation (shown only while paused).
- **Zoom − / Zoom +** — scale the cells down/up; the grid scrolls within its
  viewport when it's bigger than the screen.
- **Slower / Faster** — halve/double the tick rate.
- **Gravity − / Gravity +** — a non-Conway twist (0–8). With gravity on, each
  generation every live cell also drifts one step toward the global center of
  mass (population-conserving — a blocked cell just stays). Low values give a
  slow inward drift; high values collapse everything into a central blob. It's
  an experiment, not real Game of Life — but a fun one.
- **Click** a cell to toggle it alive/dead.
- **Click-and-drag** across cells to paint them alive.

Below the buttons, a readout shows **generation**, **live cells**, the current
**grid** size, **zoom** (pixels per cell), **speed** (target generations per
second), and **gravity** strength.

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
it at the left edge. Thanks to the JS engine it still runs at tens of
generations per second at that size. The pattern is parsed from the Golly
pattern collection (`breeder.lif`).

## Project layout

### Architecture

Elm is the **controller** (standard Model / Update / View), and the simulation
engine lives in **JavaScript** for speed. They talk over ports.

| File                   | Responsibility                                              |
| ---------------------- | ----------------------------------------------------------- |
| `Main.elm`             | Wires everything together via `Browser.element`.            |
| `src/Model.elm`        | `Model`/`Settings` (running, speed, zoom, generation, population) and the initial state. |
| `src/Messages.elm`     | The `Msg` type — every event the app can handle.            |
| `src/Update.elm`       | Handles messages; sends engine commands out the ports.      |
| `src/View.elm`         | Renders the controls, counters, and the `<canvas>` element. |
| `src/Subscriptions.elm`| Clock tick (drives stepping) and the inbound population port. |
| `src/Patterns.elm`     | The named presets and the logic for placing them on the grid. |
| `src/Ports.elm`        | Ports to/from the JS engine (load, randomize, step, zoom, population). |
| `src/Breeder.elm`      | The breeder's 4060 cells (generated from `breeder.lif`).    |
| `index.html`           | The JS engine: the `Uint8Array` world, the Conway step, canvas drawing, and mouse input. |

The world is a flat `Uint8Array` (1 = alive) held in JS, double-buffered so each
generation is computed without allocation. A cell's position is its index;
neighbors are `i ± 1`, `i ± width`, and the diagonals, with edges treated as
dead (no wrapping). Elm drives the clock: while running, `Time.every` sends a
`stepEngine` command; JS advances one generation, redraws, and reports the new
population back to Elm for the counter. Because everything runs on one thread,
the clock self-throttles to whatever the engine can sustain.

Presets are defined in `Patterns.elm` as a list of `(row, col)` offsets, an
`Anchor` (`Center`, `Top`, or `Left`) saying where to drop them, and a
`gridSize` the world switches to when loaded. To add one, append a `Pattern` to
`all` — a button is generated for it automatically.
