module World exposing (..)


import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.App
import Html.Events exposing (onClick)
import Array exposing (Array)


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
        [ text <| toString cell.id ]


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
    NoOp ->
      ( model, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
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
