module World exposing (..)


import Array exposing (Array)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.App
import Html.Events exposing (onClick)
import List
import Time exposing (Time, millisecond)


-- MODEL


type alias Model =
  { running : Bool
  , grid : Grid
  }


type alias Grid =
  Array Cell


type alias Cell =
  { id : Int
  , state : Bool
  }


init : ( Model, Cmd Msg )
init =
  ( Model False grid, Cmd.none )


grid : Grid
grid =
  (Array.repeat 100 False)
  |> Array.indexedMap Cell


-- MESSAGES


type Msg
  = ToggleCell Int
  | AgeWorld Time
  | ToggleTime


-- VIEW


view : Model -> Html Msg
view model =
  div []
      [ worldGrid model
      , controls model
      ]


worldGrid : Model -> Html Msg
worldGrid model =
  div [ style [ ("width", "300px")
              , ("margin", "0 auto")
              , ("margin-top", "100px")
              ]
      ]
      <| Array.toList (Array.map render model.grid)


render : Cell -> Html Msg
render cell =
  let
    bgcolor =
      if cell.state then
        "black"
      else
        "white"
  in
    div [ style [ ("width", "30px")
                , ("height", "30px")
                , ("outline", "1px solid")
                , ("float", "left")
                , ("background-color", bgcolor)
                ]
        , onClick <| ToggleCell cell.id
        ]
        []


controls : Model -> Html Msg
controls model =
  let
    toggleText =
      if model.running == True then "Pause Game" else "Start Game"
  in
    div []
        [ button [ onClick ToggleTime ] [ text toggleText ] ]


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleCell id ->
      ( { model | grid = Array.map (toggleCell id) model.grid }, Cmd.none )
    AgeWorld _ ->
      ( { model | grid = Array.map (age model.grid) model.grid }, Cmd.none )
    ToggleTime ->
      ( { model | running = not model.running }, Cmd.none )


toggleCell : Int -> Cell -> Cell
toggleCell id cell =
  if cell.id == id then
    { cell | state = not cell.state }
  else
    cell


age : Grid -> Cell -> Cell
age grid cell =
  let
    numNeighbors =
      neighbors grid cell.id
      |> List.filter (\c -> c.state == True)
      |> List.foldl (\_ count -> count + 1) 0

    resurrect =
      cell.state == False && numNeighbors == 3

    kill =
      cell.state == True && (numNeighbors < 2 || numNeighbors > 3)

    newState =
      if resurrect then
        True
      else if kill then
        False
      else
        cell.state
  in
    { cell | state = newState }


neighbors : Grid -> Int -> List Cell
neighbors grid index =
  let
    perimeter =
      [ index + 1 , index - 1
      , index + width, index - width
      , index + width + 1, index - width + 1
      , index + width - 1, index - width - 1
      ]

    getCell position =
      case Array.get position grid of
        Just cell ->
          cell
        Nothing ->
          -- Dead cells beyond world grid edge
          Cell 0 False
  in
    List.map getCell perimeter


width : Int
width =
  10


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.running then
    Time.every (200 * millisecond ) AgeWorld
  else
    Sub.none


-- MAIN


main : Program Never
main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
