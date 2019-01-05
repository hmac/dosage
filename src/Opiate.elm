module Opiate exposing (Model, init, view)

import Html exposing (Html, div)


type alias Model =
    {}


init : Model
init =
    {}


view : (Model -> msg) -> Model -> Html msg
view update model =
    div [] []
