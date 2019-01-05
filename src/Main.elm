module Main exposing (main)

import Browser
import Gentamicin
import Html exposing (Html, a, br, div, h2, li, nav, text, ul)
import Html.Attributes exposing (class, href)
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
    { page = Opioid Opioid.init }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetPage p ->
            { model | page = p }


view : Model -> Html Msg
view model =
    div []
        [ header model.page
        , br [] []
        , body model.page
        ]


pageName : Page -> String
pageName page =
    case page of
        Opioid _ ->
            "opioid"

        Gentamicin _ ->
            "gentamicin"


header : Page -> Html Msg
header page =
    let
        active pageStr =
            if pageName page == pageStr then
                class "active"

            else
                class ""
    in
    nav [ class "navbar navbar-expand-lg navbar-light bg-light" ]
        [ a [ class "navbar-brand", href "#" ] [ h2 [] [ text "Dosage" ] ]
        , div [ class "navbar-collapse" ]
            [ div [ class "navbar-nav" ]
                [ a
                    [ class "nav-item nav-link", active "opioid", href "#", onClick (SetPage (Opioid Opioid.init)) ]
                    [ text "Opioids" ]
                , a [ class "nav-item nav-link", active "gentamicin", href "#", onClick (SetPage (Gentamicin Gentamicin.init)) ]
                    [ text "Gentamicin" ]
                ]
            ]
        ]


body : Page -> Html Msg
body page =
    case page of
        Gentamicin model ->
            Gentamicin.view (\m -> SetPage (Gentamicin m)) model

        Opioid model ->
            Opioid.view (\m -> SetPage (Opioid m)) model
