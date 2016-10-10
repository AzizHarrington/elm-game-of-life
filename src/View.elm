module View exposing (..)


import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseOver, onMouseUp)
import Html.Lazy exposing (lazy, lazy2)


import Array
import Messages exposing (..)
import Model exposing (Model, Settings, Grid, Cell)


view : Model -> Html Msg
view model =
  div [ style [ ("-webkit-touch-callout", "none")
              , ("-webkit-user-select", "none")
              , ("-khtml-user-select", "none")
              , ("-moz-user-select", "none")
              , ("-ms-user-select", "none")
              , ("-o-user-select", "none")
              , ("user-select", "none")
              , ("margin-top", "100px")
              ]
      ]
      [ lazy controls model
      , worldGrid model
      ]


controls : Model -> Html Msg
controls model =
  let
    toggleText =
      if model.settings.running == True then "Pause Game" else "Start Game"

    cleanSlateGrid =
      List.repeat (model.settings.gridCellWidth ^ 2) False
  in
    div [ style [ ("margin", "0 auto")
                , ("margin-bottom", "25px")
                , ("width", (toString model.settings.gridPixelWidth) ++ "px")
                ]
        ]
        [ button [ onClick ToggleTime ] [ text toggleText ]
        , button [ onClick RandomizeGrid ] [ text "Randomize" ]
        , button [ onClick <| SetGrid cleanSlateGrid ] [ text "Reset" ]
        ]


worldGrid : Model -> Html Msg
worldGrid model =
  let
    lazyRender cell =
      lazy2 renderCell cell model.settings
  in
    div [ style [ ("width", (toString model.settings.gridPixelWidth) ++ "px")
                , ("height", (toString model.settings.gridPixelWidth) ++ "px")
                , ("outline", "1px solid")
                , ("margin", "0 auto")
                ]
        , onMouseDown MouseDown
        , onMouseUp MouseUp
        ]
        <| Array.toList (Array.map lazyRender model.grid)


renderCell : Cell -> Settings -> Html Msg
renderCell cell settings =
  let
    bgcolor =
      if cell.isAlive then
        "black"
      else
        "white"

    maybeAnimate id =
      if settings.highlight then AnimateCell id else NoOp
  in
    div [ style [ ("width", (toString settings.cellPixelWidth) ++ "px")
                , ("height", (toString settings.cellPixelWidth) ++ "px")
                , ("float", "left")
                , ("background-color", bgcolor)
                ]
        , onClick <| ToggleCell cell.id
        , onMouseOver <| maybeAnimate cell.id
        ]
        []
