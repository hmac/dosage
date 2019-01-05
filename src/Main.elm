module Main exposing (main)

import Browser
import Gentamicin
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Opioid


type alias Model =
    { page : Page }


type Page
    = Gentamicin Gentamicin.Model
    | Opioid Opioid.Model


type Msg
    = SetPage Page


main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    { page = Gentamicin Gentamicin.init }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetPage p ->
            { model | page = p }


view : Model -> Html Msg
view model =
    div []
        [ header
        , body model.page
        ]


header : Html Msg
header =
    div []
        [ a [ href "#", onClick (SetPage (Gentamicin Gentamicin.init)) ] [ text "Gentamicin" ]
        , a [ href "#", onClick (SetPage (Opioid Opioid.init)) ] [ text "Opioids" ]
        ]


body : Page -> Html Msg
body page =
    case page of
        Gentamicin model ->
            Gentamicin.view (\m -> SetPage (Gentamicin m)) model

        Opioid model ->
            Opioid.view (\m -> SetPage (Opioid m)) model
