# elm-game-of-life

[Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)
implemented in [Elm](https://elm-lang.org/). A 100×100 grid of cells evolves
each tick according to the classic rules:

- A live cell with 2 or 3 live neighbors survives.
- A dead cell with exactly 3 live neighbors becomes alive.
- All other cells die or stay dead.

Neighbors are the 8 surrounding cells. The grid does not wrap — cells past the
edges are treated as dead.

## Running it

The project uses **Elm 0.19.1**. A local copy is installed under
`node_modules` (via `npm install`), so you don't need a global Elm.

Build a standalone HTML file and open it:

```sh
./node_modules/.bin/elm make Main.elm --output=index.html
open index.html
```

Or run the live-reloading dev server and visit the printed URL:

```sh
./node_modules/.bin/elm reactor
```

`index.html` is a build artifact and is gitignored.

To run the dev server on a specific port, pass `--port` (e.g. `--port=5555`)
and visit `http://localhost:5555`.

## Controls

- **Start / Pause** — run or pause the simulation. It auto-pauses when no live
  cells remain.
- **Randomize** — fill the grid with a random pattern.
- **Reset** — clear the grid.
- **Step** — advance a single generation (shown only while paused).
- **Click** a cell to toggle it alive/dead.
- **Click-and-drag** across cells to paint them alive.

Below the buttons, a readout shows the current **generation** and **live cell**
counts. The generation counter advances each tick (and on Step) and resets to 0
whenever you load a preset, randomize, or reset.

## Presets

Buttons that load a famous starting pattern, each showing off a different kind
of behavior. Loading one clears the grid and pauses, so hit **Start** to watch.

| Preset           | What it does                                                                 |
| ---------------- | --------------------------------------------------------------------------- |
| **Glider**       | The smallest spaceship — crawls diagonally across the grid.                 |
| **Glider Gun**   | A Gosper glider gun: stays put and fires a new glider every 30 generations. |
| **Pulsar**       | A large, symmetric period-3 oscillator.                                     |
| **Acorn**        | A 7-cell methuselah that erupts into long-running chaos.                    |
| **Puffer Train** | A spaceship that travels down the grid leaving a trail of debris.          |

On this finite, non-wrapping grid the moving patterns eventually reach an edge
and their debris settles into still lifes — a random grid likewise decays into
stable "ash." That's expected Game of Life behavior, not a bug.

## Project layout

The code follows the standard Elm Architecture (Model / Update / View),
split into focused modules:

| File                   | Responsibility                                              |
| ---------------------- | ----------------------------------------------------------- |
| `Main.elm`             | Wires everything together via `Browser.element`.            |
| `src/Model.elm`        | `Model` and `Settings` types, plus the initial state.       |
| `src/Messages.elm`     | The `Msg` type — every event the app can handle.            |
| `src/Update.elm`       | State transitions, including the Game of Life rules (`ageGrid`). |
| `src/View.elm`         | Renders the grid, controls, and counters to HTML.          |
| `src/Subscriptions.elm`| Drives the clock tick that ages the world while running.    |
| `src/Patterns.elm`     | The named presets and the logic for placing them on the grid. |

The grid is stored as an `Array Cell`, where each `Cell` knows its index `id`
and whether it `isAlive`. Cell position is derived from the index using the
grid width (e.g. neighbors are `id ± 1`, `id ± width`, etc.).

Presets are defined in `Patterns.elm` as a list of `(row, col)` offsets plus an
`Anchor` (`Center` or `Top`) saying where to drop them. To add one, append a
`Pattern` to `all` — a button is generated for it automatically.
