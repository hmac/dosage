module Drug exposing (Drug(..), all, decode, encode, fromString, toString)

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


all : List Drug
all =
    [ Codeine, Dihydrocodeine, Hydromorphone, Morphine, Oxycodone, Tapentadol, Tramadol ]


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
