module Drug exposing (Drug(..), all, decode, encode, fromString, potency, toString)

import Json.Decode
import Json.Encode


type Drug
    = Codeine
    | Dihydrocodeine
    | Hydromorphone
    | Morphine
    | Oxycodone
    | Tapentadol
    | Tramadol
    | SubcutDiamorphine
    | SubcutMorphine
    | SubcutOxycodone


potency : Drug -> Float
potency d =
    case d of
        Codeine ->
            0.1

        Dihydrocodeine ->
            0.1

        Hydromorphone ->
            7.5

        Morphine ->
            1

        Oxycodone ->
            2

        Tapentadol ->
            0.4

        Tramadol ->
            0.15

        SubcutDiamorphine ->
            3

        SubcutMorphine ->
            2

        SubcutOxycodone ->
            4


all : List Drug
all =
    [ Codeine
    , Dihydrocodeine
    , Hydromorphone
    , Morphine
    , Oxycodone
    , Tapentadol
    , Tramadol
    , SubcutDiamorphine
    , SubcutMorphine
    , SubcutOxycodone
    ]


toString : Drug -> String
toString d =
    case d of
        Codeine ->
            "Codeine"

        Dihydrocodeine ->
            "Dihydrocodeine"

        Hydromorphone ->
            "Hydromorphone"

        Morphine ->
            "Morphine"

        Oxycodone ->
            "Oxycodone"

        Tapentadol ->
            "Tapentadol"

        Tramadol ->
            "Tramadol"

        SubcutDiamorphine ->
            "Subcutaneous Diamorphine"

        SubcutMorphine ->
            "Subcutaneous Morphine"

        SubcutOxycodone ->
            "Subcutaneous Oxycodone"


fromString : String -> Maybe Drug
fromString str =
    case str of
        "Codeine" ->
            Just Codeine

        "Dihydrocodeine" ->
            Just Dihydrocodeine

        "Hydromorphone" ->
            Just Hydromorphone

        "Morphine" ->
            Just Morphine

        "Oxycodone" ->
            Just Oxycodone

        "Tapentadol" ->
            Just Tapentadol

        "Tramadol" ->
            Just Tramadol

        "Subcutaneous Diamorphine" ->
            Just SubcutDiamorphine

        "Subcutaneous Morphine" ->
            Just SubcutMorphine

        "Subcutaneous Oxycodone" ->
            Just SubcutOxycodone

        _ ->
            Nothing


encode : Drug -> Json.Encode.Value
encode d =
    Json.Encode.string (toString d)


decode : Json.Decode.Decoder Drug
decode =
    let
        go : String -> Json.Decode.Decoder Drug
        go s =
            case fromString s of
                Just d ->
                    Json.Decode.succeed d

                Nothing ->
                    Json.Decode.fail "unrecognised drug"
    in
    Json.Decode.string |> Json.Decode.andThen go
