module View exposing (..)


import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseEnter, onMouseDown, onMouseOut, onMouseUp)
import Html.Lazy exposing (lazy, lazy2)


import Array
import Messages exposing (..)
import Model exposing (Model, Settings, Grid, Cell)
import Patterns


view : Model -> Html Msg
view model =
  div [ style "-webkit-touch-callout" "none"
      , style "-webkit-user-select" "none"
      , style "-khtml-user-select" "none"
      , style "-moz-user-select" "none"
      , style "-ms-user-select" "none"
      , style "-o-user-select" "none"
      , style "user-select" "none"
      , style "margin-top" "100px"
      ]
      [ lazy controls model
      , worldGrid model
      ]


controls : Model -> Html Msg
controls model =
  let
    toggleText =
      if model.settings.running then "Pause" else "Start"

    cleanSlateGrid =
      List.repeat (model.settings.gridCellWidth ^ 2) False

    step =
      if not model.settings.running then
        button [ onClick Step ] [ text "Step" ]
      else
        div [] []

    patternButton pattern =
      button [ onClick <| LoadPattern pattern ] [ text pattern.name ]

    liveCells =
      model.grid
        |> Array.toList
        |> List.filter .isAlive
        |> List.length

    counters =
      div [ style "margin-top" "10px"
          , style "font-family" "monospace"
          ]
          [ text <|
              "Generation: "
                ++ String.fromInt model.generation
                ++ "    Live cells: "
                ++ String.fromInt liveCells
          ]
  in
    div [ style "margin" "0 auto"
        , style "margin-bottom" "25px"
        , style "width" (String.fromInt model.settings.gridPixelWidth ++ "px")
        ]
        ([ button [ onClick ToggleTime ] [ text toggleText ]
         , button [ onClick RandomizeGrid ] [ text "Randomize" ]
         , button [ onClick <| SetGrid cleanSlateGrid ] [ text "Reset" ]
         , step
         ]
            ++ List.map patternButton Patterns.all
            ++ [ counters ]
        )


worldGrid : Model -> Html Msg
worldGrid model =
  let
    lazyRender cell =
      lazy2 renderCell cell model.settings
  in
    div [ style "width" (String.fromInt model.settings.gridPixelWidth ++ "px")
        , style "height" (String.fromInt model.settings.gridPixelWidth ++ "px")
        , style "margin" "0 auto"
        , onMouseDown MouseDown
        , onMouseUp MouseUp
        ]
        (Array.toList (Array.map lazyRender model.grid))


renderCell : Cell -> Settings -> Html Msg
renderCell cell settings =
  let
    bgcolor =
      if cell.isAlive then
        "black"
      else
        "white"

    textcolor =
      if cell.isAlive then
        "white"
      else
        "black"

    maybeAnimate id =
      if settings.highlight then AnimateCell id else NoOp
  in
    div [ style "width" (String.fromFloat settings.cellPixelWidth ++ "px")
        , style "height" (String.fromFloat settings.cellPixelWidth ++ "px")
        , style "float" "left"
        , style "background-color" bgcolor
        , style "color" textcolor
        , style "outline" "1px solid LightGray"
        , onClick <| ToggleCell cell.id
        , onMouseOut <| maybeAnimate cell.id
        , onMouseEnter <| maybeAnimate cell.id
        ]
        []
