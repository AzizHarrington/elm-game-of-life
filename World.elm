module World exposing (..)


import Array exposing (Array)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.App
import Html.Events exposing (onClick)
import List
import Time exposing (Time, millisecond)


-- MODEL


type alias Model =
  Array Cell


type alias Cell =
  { id : Int
  , state : Bool
  }


init : ( Model, Cmd Msg )
init =
  ( grid, Cmd.none )


grid : Model
grid =
  (Array.repeat 100 False)
  |> Array.indexedMap Cell


-- MESSAGES


type Msg
  = Toggle Int
  | AgeWorld Time
  | NoOp


-- VIEW


view : Model -> Html Msg
view model =
  div [ style [ ("width", "300px")
              , ("margin", "0 auto")
              , ("margin-top", "100px")
              ]
      ]
      <| Array.toList (Array.map render model)


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
        , onClick <| Toggle cell.id
        ]
        []


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Toggle id ->
      let
        toggleState cell =
          if cell.id == id then
            { cell | state = not cell.state }
          else
            cell
      in
        ( Array.map toggleState model, Cmd.none )
    AgeWorld _ ->
      ( Array.map (age model) model, Cmd.none )
    NoOp ->
      ( model, Cmd.none )


age : Model -> Cell -> Cell
age model cell =
  let
    numNeighbors =
      neighbors model cell.id
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


neighbors : Model -> Int -> List Cell
neighbors model index =
  let
    perimeter =
      [ index + 1 , index - 1
      , index + width, index - width
      , index + width + 1, index - width + 1
      , index + width - 1, index - width - 1
      ]

    getCell position =
      case Array.get position model of
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
  Time.every (3000 * millisecond ) AgeWorld


-- MAIN


main : Program Never
main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
