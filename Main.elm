module Main exposing (main)


import Browser
import Messages exposing (Msg)
import Model exposing (Model, init)
import View exposing (view)
import Update exposing (update)
import Subscriptions exposing (subscriptions)


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
