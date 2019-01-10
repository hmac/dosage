module Opioid exposing (Model, init, view)

import Drug exposing (Drug, potency)
import Html
    exposing
        ( Html
        , a
        , br
        , code
        , div
        , h3
        , h4
        , input
        , option
        , p
        , select
        , strong
        , text
        )
import Html.Attributes
    exposing
        ( disabled
        , href
        , placeholder
        , readonly
        , selected
        , style
        , value
        )
import Html.Events exposing (on, onInput, targetValue)
import Json.Decode
import Json.Encode


type alias Model =
    { fromDrug : Drug, fromAmount : Float, toDrug : Drug }


init : Model
init =
    { fromDrug = Drug.Codeine, fromAmount = 1, toDrug = Drug.Tapentadol }


view : (Model -> msg) -> Model -> Html msg
view update model =
    div []
        [ h3 [] [ text "Convert" ]
        , p []
            [ input [ onInput (update << setFromAmount model), placeholder "1" ] []
            , text " mg of "
            , drugSelect model.fromDrug (update << setFromDrug model)
            ]
        , p []
            [ input
                [ disabled True
                , readonly True
                , value
                    (String.fromFloat
                        (dose model.fromDrug model.fromAmount model.toDrug)
                    )
                ]
                []
            , text " mg of "
            , drugSelect model.toDrug (update << setToDrug model)
            ]
        , explain model.fromDrug model.fromAmount model.toDrug
        , br [] []
        , note
        ]


modelToString : Model -> String
modelToString model =
    let
        encodedModel =
            Json.Encode.object
                [ ( "fromDrug", Drug.encode model.fromDrug )
                , ( "fromAmount", Json.Encode.float model.fromAmount )
                , ( "toDrug", Drug.encode model.toDrug )
                ]
    in
    Json.Encode.encode 2 encodedModel


setFromDrug : Model -> String -> Model
setFromDrug model drug =
    case Drug.fromString drug of
        Just d ->
            { model | fromDrug = d }

        Nothing ->
            model


setToDrug : Model -> String -> Model
setToDrug model drug =
    case Drug.fromString drug of
        Just d ->
            { model | toDrug = d }

        Nothing ->
            model


setFromAmount : Model -> String -> Model
setFromAmount model str =
    case String.toFloat str of
        Just amount ->
            { model | fromAmount = amount }

        Nothing ->
            model


drugSelect : Drug -> (String -> msg) -> Html msg
drugSelect current onChange =
    let
        drugOption d =
            option [ value (Drug.toString d), selected (d == current) ] [ text (Drug.toString d) ]
    in
    select [ onInput onChange ] (List.map drugOption Drug.all)


dose : Drug -> Float -> Drug -> Float
dose from amount to =
    let
        inMorphine =
            amount * potency from
    in
    inMorphine / potency to


explain : Drug -> Float -> Drug -> Html msg
explain from fromAmount to =
    div []
        [ strong [] [ text (Drug.toString from) ]
        , text " has a potency ratio of "
        , code [] [ text (String.fromFloat (potency from)) ]
        , text ", "

        -- , br [] []
        , text "therefore "
        , code [] [ text (String.fromFloat fromAmount) ]
        , text " mg of "
        , strong [] [ text (Drug.toString from) ]
        , text " is equivalent to "
        , code [] [ text (equationToMorphine fromAmount from) ]
        , text " mg of "
        , strong [] [ text "Morphine." ]
        , br [] []
        , strong [] [ text (Drug.toString to) ]
        , text " has a potency ratio of "
        , code [] [ text (String.fromFloat (potency to)) ]
        , text ", "

        -- , br [] []
        , text "therefore "
        , code [] [ text (String.fromFloat (fromAmount * potency from)) ]
        , text " mg of "
        , strong [] [ text "Morphine" ]
        , text " is equivalent to "
        , code [] [ text (equationFromMorphine (fromAmount * potency from) to) ]
        , text " mg of "
        , strong [] [ text (Drug.toString to) ]
        ]


explainPotency : Drug -> Html msg
explainPotency d =
    div []
        [ p []
            [ strong [] [ text (Drug.toString d) ]
            , text " has a potency ratio of "
            , code [] [ text (String.fromFloat (potency d)) ]
            ]
        ]


equationToMorphine : Float -> Drug -> String
equationToMorphine fromAmount drug =
    String.fromFloat fromAmount ++ " * " ++ String.fromFloat (potency drug) ++ " = " ++ String.fromFloat (fromAmount * potency drug)


equationFromMorphine : Float -> Drug -> String
equationFromMorphine fromAmount drug =
    String.fromFloat fromAmount ++ " / " ++ String.fromFloat (potency drug) ++ " = " ++ String.fromFloat (fromAmount / potency drug)


note : Html msg
note =
    div []
        [ h4 [] [ text "Note" ]
        , p [ style "font-size" "10pt" ]
            [ text "Individuals metabolise different opiods at different rates."
            , text " When switching opioids, the RCoA "
            , a
                [ href
                    "https://www.rcoa.ac.uk/faculty-of-pain-medicine/opioids-aware/structured-approach-to-prescribing/dose-equivalents-and-changing-opioids"
                ]
                [ text "recommends" ]
            , text " reducing the dose of the new opioid by 25-50%."
            , text " This applies particularly for elderly or frail patients, or where the dosage is equivalent to more than 500mg of oral morphine in 24 hours."
            , text " After conversion, the new opioid should be titrated according to the individual's response, taking into account side-effects and efficacy"
            ]
        ]
