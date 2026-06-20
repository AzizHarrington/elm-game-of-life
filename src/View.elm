module View exposing (..)


import Html exposing (Html, button, canvas, div, text)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick)


import Messages exposing (..)
import Model exposing (Model, Settings)
import Patterns


view : Model -> Html Msg
view model =
  div [ style "-webkit-user-select" "none"
      , style "-moz-user-select" "none"
      , style "user-select" "none"
      , style "margin-top" "40px"
      , style "text-align" "center"
      ]
      [ controls model
      , worldGrid model
      ]


controls : Model -> Html Msg
controls model =
  let
    toggleText =
      if model.settings.running then "Pause" else "Start"

    step =
      if not model.settings.running then
        button [ onClick Step ] [ text "Step" ]
      else
        text ""

    patternButton pattern =
      button [ onClick <| LoadPattern pattern ] [ text pattern.name ]

    perSecond =
      round (1000 / model.settings.timeStep)

    counters =
      div [ style "margin-top" "10px"
          , style "font-family" "monospace"
          ]
          [ text <|
              "Generation: "
                ++ String.fromInt model.generation
                ++ "    Live cells: "
                ++ String.fromInt model.population
                ++ "    Grid: "
                ++ String.fromInt model.settings.gridCellWidth
                ++ "²    Zoom: "
                ++ String.fromFloat model.settings.cellPixelWidth
                ++ "px    Speed: "
                ++ String.fromInt perSecond
                ++ "/s    Gravity: "
                ++ String.fromInt model.gravity
          ]
  in
    div [ style "margin-bottom" "16px" ]
        [ div []
            ([ button [ onClick ToggleTime ] [ text toggleText ]
             , button [ onClick RandomizeGrid ] [ text "Randomize" ]
             , button [ onClick ResetGrid ] [ text "Reset" ]
             , step
             ]
                ++ List.map patternButton Patterns.all
            )
        , div [ style "margin-top" "8px" ]
            [ button [ onClick ZoomOut ] [ text "Zoom −" ]
            , button [ onClick ZoomIn ] [ text "Zoom +" ]
            , button [ onClick SlowDown ] [ text "Slower" ]
            , button [ onClick SpeedUp ] [ text "Faster" ]
            , button [ onClick GravityDown ] [ text "Gravity −" ]
            , button [ onClick GravityUp ] [ text "Gravity +" ]
            , button [ onClick ToggleReflect ]
                [ text (if model.reflect then "Walls: Reflecting" else "Walls: Dead") ]
            ]
        , counters
        ]


-- The grid is drawn by JS onto this canvas (see index.html). Its pixel size and
-- contents come through the drawGrid port; here we just provide the element and
-- a stable id for JS to target.
worldGrid : Model -> Html Msg
worldGrid model =
  div
    [ style "display" "inline-block"
    , style "overflow" "auto"
    , style "max-width" "94vw"
    , style "max-height" "78vh"
    , style "border" "1px solid #ccc"
    ]
    [ canvas
        [ id "grid"
        , style "image-rendering" "pixelated"
        , style "display" "block"
        ]
        []
    ]
