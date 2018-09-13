-- TODO: Use Ratio instead of Float
-- TODO: What do we do if patient is less than 5 feet tall?
-- TODO: Display units for creatinine clearance


module Main exposing (main)

import Browser
import Html
    exposing
        ( Html
        , br
        , button
        , div
        , form
        , h4
        , input
        , label
        , option
        , p
        , pre
        , select
        , table
        , td
        , text
        , tr
        )
import Html.Attributes exposing (checked, class, selected, type_, value)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as Json exposing (andThen, map)
import List
import Maybe
import String as S exposing (fromFloat, fromInt)
import Tuple



-- Main


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Model =
    { sex : Sex
    , defaultHeightUnit : HeightUnit
    , height : Maybe Height
    , age : Maybe Age
    , weight : Maybe Weight
    , serumCreatinine : Maybe SerumCreatinine
    , dosage : Dosage
    , gentamicinMeasurement : ( Int, Maybe Float )
    }


type Sex
    = Male
    | Female


type Height
    = Height HeightUnit Int


type HeightUnit
    = Cm
    | Inch


type Weight
    = Weight Int


type Age
    = Age Int


type SerumCreatinine
    = SerumCreatinine Float


type Clearance
    = Clearance Float


type Dosage
    = Daily5mg
    | Daily7mg
    | Divided


init : Model
init =
    { sex = Female
    , defaultHeightUnit = Inch
    , height = Nothing
    , age = Nothing
    , weight = Nothing
    , serumCreatinine = Nothing
    , dosage = Daily5mg
    , gentamicinMeasurement = ( 6, Nothing )
    }


type Msg
    = SetSex (Maybe Sex)
    | SetHeightValue HeightUnit String
    | SetHeightUnit String
    | SetWeight String
    | SetAge String
    | SetSerumCreatinine String
    | SetDosage (Maybe Dosage)
    | SetGentamicinMeasurement (Maybe Int) (Maybe Float)


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetSex mSex ->
            case mSex of
                Just s ->
                    { model | sex = s }

                Nothing ->
                    model

        SetHeightValue unit heightStr ->
            { model | height = Maybe.map (Height unit) (String.toInt heightStr) }

        SetHeightUnit unitStr ->
            case stringToHeightUnit unitStr of
                Just u ->
                    case model.height of
                        Just (Height _ h) ->
                            { model | height = Just (Height u h), defaultHeightUnit = u }

                        Nothing ->
                            { model | defaultHeightUnit = u }

                Nothing ->
                    model

        SetWeight weightStr ->
            { model | weight = Maybe.map Weight (String.toInt weightStr) }

        SetAge ageStr ->
            { model | age = Maybe.map Age (String.toInt ageStr) }

        SetSerumCreatinine scStr ->
            { model | serumCreatinine = Maybe.map SerumCreatinine (String.toFloat scStr) }

        SetDosage mDosage ->
            case mDosage of
                Just d ->
                    { model | dosage = d }

                Nothing ->
                    model

        SetGentamicinMeasurement mHour mLevel ->
            case mHour of
                Nothing ->
                    model

                Just hour ->
                    { model | gentamicinMeasurement = ( hour, mLevel ) }


view : Model -> Html.Html Msg
view model =
    let
        obese =
            case isObese model of
                True ->
                    "Yes"

                False ->
                    "No"

        mkTable : List ( String, Html.Html Msg ) -> Html.Html Msg
        mkTable elems =
            let
                rows =
                    List.map toRow elems

                toRow ( name, value ) =
                    tr [] [ td [] [ label [] [ text name ] ], td [] [ value ] ]
            in
            table [] rows

        baseInputs =
            [ ( "Sex", sexInput )
            , ( "Height", heightInput )
            , ( "Age", ageInput )
            , ( "Weight", weightInput )
            , ( "Serum Creatinine", serumCreatinineInput )
            , ( "Dosage", dosageInput )
            ]

        inputs =
            if model.dosage == Daily7mg then
                baseInputs ++ [ ( "Gentamicin", gentamicinLevelInput ) ]

            else
                baseInputs

        clearance =
            maybeToString S.fromFloat (Maybe.map (\(Clearance c) -> roundFloat 2 c) (creatinineClearance model))

        weight =
            if isObese model then
                correctedBodyWeight model

            else
                idealBodyWeight model

        weightStr =
            case weight of
                Nothing ->
                    ""

                Just w ->
                    if isObese model then
                        S.fromInt (round w) ++ " (corrected bodyweight)"

                    else
                        S.fromInt (round w) ++ " (ideal bodyweight)"

        dailyInitialDose =
            case weight of
                Nothing ->
                    Nothing

                Just w ->
                    let
                        mg =
                            List.range 3 5
                    in
                    mg |> List.map (\e -> toFloat e * w / 3) |> List.maximum

        output =
            [ h4 [] [ text "Calculated attributes" ]
            , table [ class "table" ]
                [ tr [] [ td [] [ text "Obese" ], td [] [ text obese ] ]
                , tr [] [ td [] [ text "Weight" ], td [] [ text weightStr ] ]
                , tr [] [ td [] [ text "Creatinine Clearance" ], td [] [ text clearance ] ]
                ]
            , div []
                [ h4 [] [ text "Dosing Regimen" ]
                , dosageInstruction model
                ]
            ]
    in
    div []
        [ div []
            [ h4 [] [ text "Enter patient details" ]
            , form [ class "col-auto" ]
                [ mkTable (List.map (\( n, f ) -> ( n, f model )) inputs)
                ]
            ]
        , div [] output
        ]


dosageInstruction : Model -> Html.Html Msg
dosageInstruction model =
    let
        weight =
            if isObese model then
                correctedBodyWeight

            else
                idealBodyWeight

        ( initial, note, following ) =
            case weight model of
                Nothing ->
                    ( "", "", "" )

                Just w ->
                    case model.dosage of
                        Daily5mg ->
                            daily5mgDosageInstruction model w

                        Daily7mg ->
                            daily7mgDosageInstruction model w

                        Divided ->
                            dividedDosageInstruction model w
    in
    table [ class "table" ]
        [ tr []
            [ td [] [ text "Initial dose" ]
            , td [] [ text initial ]
            ]
        , tr []
            [ td [] [ text "Note" ]
            , td [] [ text note ]
            ]
        , tr []
            [ td [] [ text "Further doses" ]
            , td [] [ text following ]
            ]
        ]



-- Dosage
-- Regimen describes the dosing regimen (how much to give and when)
-- base:      the per-Kg calculated dosage
-- initial:   the actual amount to give as the first dose (base * weight)
-- following: instructions for the next dose


type alias Regimen =
    { base : Dose, initial : Dose, following : ( Hour, DoseInstruction ), note : Maybe String }


type alias Hour =
    Int


type alias Dose =
    ( Int, Int )


type DoseInstruction
    = Dose
    | Other String


daily : Model -> Float -> Maybe Regimen
daily model weight =
    case creatinineClearance model of
        Nothing ->
            Nothing

        Just (Clearance cc) ->
            let
                perKg =
                    if cc < 20.0 then
                        ( 2, 3 )

                    else
                        ( 5, 7 )

                note =
                    if cc < 20.0 then
                        Just "poor kidney function"

                    else
                        Nothing

                ( min, max ) =
                    let
                        f =
                            \x -> round (x * weight)
                    in
                    Tuple.mapBoth f f perKg

                following =
                    if cc >= 60.0 then
                        ( 24, Dose )

                    else if cc >= 40.0 then
                        ( 36, Dose )

                    else if cc >= 20.0 then
                        ( 48, Dose )

                    else
                        ( 48, Other "take gentamicin levels; apply next dose when levels fall to < 1 µmol/L" )
            in
            Just { base = perKg, initial = ( min, max ), following = following, note = note }


daily5mgDosageInstruction : Model -> Float -> ( String, String, String )
daily5mgDosageInstruction model weight =
    case daily model weight of
        Nothing ->
            ( "", "", "" )

        Just { base, initial, following, note } ->
            let
                initialStr =
                    let
                        ( min, _ ) =
                            initial
                    in
                    S.fromInt min ++ "mg"

                rangeStr ( a, b ) =
                    S.fromInt a ++ "-" ++ S.fromInt b ++ " mg"

                followingStr =
                    case following of
                        ( hour, Dose ) ->
                            "Dose every " ++ S.fromInt hour ++ " hours"

                        ( hour, Other s ) ->
                            "At " ++ S.fromInt hour ++ " hours: " ++ s
            in
            ( initialStr, Maybe.withDefault "" note, followingStr )



-- TODO
-- Display nomogram and show where measurement lies on it


daily7mgDosageInstruction : Model -> Float -> ( String, String, String )
daily7mgDosageInstruction model weight =
    case daily model weight of
        Nothing ->
            ( "", "", "" )

        Just { base, initial } ->
            let
                ( hour, mLevel ) =
                    model.gentamicinMeasurement

                initialStr =
                    let
                        ( _, max ) =
                            initial
                    in
                    S.fromInt max ++ "mg"

                area =
                    Maybe.map (nomogramArea (toFloat hour)) mLevel

                nextDose =
                    case area of
                        Nothing ->
                            ""

                        Just H48 ->
                            "Give next dose 48 hours after initial dose"

                        Just H36 ->
                            "Give next dose 36 hours after initial dose"

                        Just H24 ->
                            "Give next dose 24 hours after initial dose"

                        Just Custom ->
                            "Resume regimen once gentamicin levels fall to < 1mg/L"
            in
            ( initialStr, "Take gentamicin levels 6-14 hours after first dose", nextDose )


dividedDosageInstruction : Model -> Float -> ( String, String, String )
dividedDosageInstruction model weight =
    let
        ( min, max ) =
            ( 3 * weight / 3, 5 * weight / 3 )

        range =
            (min |> round |> S.fromInt)
                ++ "-"
                ++ (max |> round |> S.fromInt)
                ++ " mg"
    in
    ( range ++ " every 8 hours (from base dose of 3-5 mg/kg"
    , ""
    , S.join "\n"
        [ "After 1 hour, gentamicin levels should be 5-10 mg/L"
        , "After 24 hours, gentamicin levels should be < 2 mg/L"
        ]
    )



-- Inputs


sexInput : Model -> Html.Html Msg
sexInput { sex } =
    select [ class "form-control-lg", on "change" (map (SetSex << stringToSex) targetValue) ]
        [ option [ value "male", selected (sex == Male) ] [ text "Male" ]
        , option [ value "female", selected (sex == Female) ] [ text "Female" ]
        ]


heightInput : Model -> Html.Html Msg
heightInput { height, defaultHeightUnit } =
    let
        unit =
            Maybe.withDefault defaultHeightUnit (Maybe.map (\(Height u _) -> u) height)

        h =
            Maybe.map (\(Height _ h_) -> h_) height
    in
    div [ class "input-group" ]
        [ input [ type_ "number", class "form-control-lg", onInput (SetHeightValue unit) ] []
        , div [ class "input-group-append" ]
            [ div [ class "input-group-text" ]
                [ text (Maybe.withDefault "" (Maybe.map (\s -> " (" ++ s ++ ")") (Maybe.map heightToString height)))
                ]
            , select [ on "change" (map SetHeightUnit targetValue) ]
                [ option [ selected (unit == Cm), value "cm" ] [ text "Centimetres" ]
                , option [ selected (unit == Inch), value "inch" ] [ text "Inches" ]
                ]
            ]
        ]


weightInput : Model -> Html.Html Msg
weightInput { weight } =
    let
        weightStr =
            Maybe.withDefault "" (Maybe.map (\(Weight w) -> S.fromInt w) weight)
    in
    div [ class "input-group" ]
        [ input [ type_ "number", class "form-control-lg", onInput SetWeight ] []
        , div [ class "input-group-append" ]
            [ div [ class "input-group-text" ] [ text "kg" ] ]
        ]


ageInput : Model -> Html.Html Msg
ageInput { age } =
    let
        ageStr =
            Maybe.withDefault "" (Maybe.map (\(Age a) -> S.fromInt a) age)
    in
    div [ class "input-group" ]
        [ input [ type_ "number", class "form-control-lg", onInput SetAge ] []
        , div [ class "input-group-append" ]
            [ div [ class "input-group-text" ] [ text "years" ]
            ]
        ]


serumCreatinineInput : Model -> Html.Html Msg
serumCreatinineInput { serumCreatinine } =
    let
        scStr =
            Maybe.withDefault "" (Maybe.map (\(SerumCreatinine sc) -> S.fromFloat sc) serumCreatinine)
    in
    div [ class "input-group" ]
        [ input [ type_ "number", class "form-control-lg", onInput SetSerumCreatinine ] []
        , div [ class "input-group-append" ]
            [ div [ class "input-group-text" ] [ text " µmol/L" ] ]
        ]


dosageInput : Model -> Html.Html Msg
dosageInput { dosage } =
    select [ class "form-control-lg", on "change" (map (SetDosage << stringToDosage) targetValue) ]
        [ option [ value "daily_5mg", selected (dosage == Daily5mg) ] [ text "Daily (5 mg)" ]
        , option [ value "daily_7mg", selected (dosage == Daily7mg) ] [ text "Daily (7 mg)" ]
        , option [ value "divided", selected (dosage == Divided) ] [ text "Divided" ]
        ]


gentamicinLevelInput : Model -> Html.Html Msg
gentamicinLevelInput { gentamicinMeasurement } =
    let
        ( hour, mLevel ) =
            gentamicinMeasurement
    in
    case mLevel of
        Nothing ->
            let
                hourHandler =
                    map (\hStr -> SetGentamicinMeasurement (S.toInt hStr) Nothing) targetValue

                levelHandler s =
                    SetGentamicinMeasurement (Just hour) (S.toFloat s)
            in
            div [ class "input-group" ]
                [ input [ type_ "number", class "form-control-lg", onInput levelHandler ] []
                , div [ class "input-group-append" ]
                    [ div [ class "input-group-text" ] [ text " µg/mL" ]
                    , select [ class "form-control-lg", on "change" hourHandler ]
                        [ option [ value "6", selected (hour == 6) ] [ text "after 6 hours" ]
                        , option [ value "7", selected (hour == 7) ] [ text "after 7 hours" ]
                        , option [ value "8", selected (hour == 8) ] [ text "after 8 hours" ]
                        , option [ value "9", selected (hour == 9) ] [ text "after 9 hours" ]
                        , option [ value "10", selected (hour == 10) ] [ text "after 10 hours" ]
                        , option [ value "11", selected (hour == 11) ] [ text "after 11 hours" ]
                        , option [ value "12", selected (hour == 12) ] [ text "after 12 hours" ]
                        , option [ value "13", selected (hour == 13) ] [ text "after 13 hours" ]
                        , option [ value "14", selected (hour == 14) ] [ text "after 14 hours" ]
                        ]
                    ]
                ]

        Just level ->
            let
                hourHandler =
                    map (\hStr -> SetGentamicinMeasurement (S.toInt hStr) (Just level)) targetValue

                levelHandler s =
                    SetGentamicinMeasurement (Just hour) (S.toFloat s)
            in
            div [ class "input-group" ]
                [ input [ type_ "number", class "form-control-lg", onInput levelHandler ] []
                , div [ class "input-group-append" ]
                    [ div [ class "input-group-text" ] [ text "µg/mL" ]
                    , select [ class "form-control-lg", on "change" hourHandler ]
                        [ option [ value "6", selected (hour == 6) ] [ text "after 6 hours" ]
                        , option [ value "7", selected (hour == 7) ] [ text "after 7 hours" ]
                        , option [ value "8", selected (hour == 8) ] [ text "after 8 hours" ]
                        , option [ value "9", selected (hour == 9) ] [ text "after 9 hours" ]
                        , option [ value "10", selected (hour == 10) ] [ text "after 10 hours" ]
                        , option [ value "11", selected (hour == 11) ] [ text "after 11 hours" ]
                        , option [ value "12", selected (hour == 12) ] [ text "after 12 hours" ]
                        , option [ value "13", selected (hour == 13) ] [ text "after 13 hours" ]
                        , option [ value "14", selected (hour == 14) ] [ text "after 14 hours" ]
                        ]
                    ]
                ]



-- Specific Utils


heightToString : Height -> String
heightToString (Height unit h) =
    case unit of
        Cm ->
            cmToString h

        Inch ->
            inchToString h


inchToString : Int -> String
inchToString h =
    let
        feet =
            h // 12

        inches =
            remainderBy 12 h
    in
    if feet == 0 then
        S.fromInt h ++ " inches"

    else
        S.fromInt feet ++ " feet " ++ S.fromInt inches ++ " inches"


cmToString : Int -> String
cmToString h =
    let
        metres =
            h // 100

        cm =
            remainderBy 100 h
    in
    if metres == 0 then
        S.fromInt h ++ " cm"

    else
        S.fromInt metres ++ "m " ++ S.fromInt cm ++ "cm"


stringToHeightUnit : String -> Maybe HeightUnit
stringToHeightUnit str =
    case str of
        "cm" ->
            Just Cm

        "inch" ->
            Just Inch

        _ ->
            Nothing


stringToSex : String -> Maybe Sex
stringToSex str =
    case str of
        "male" ->
            Just Male

        "female" ->
            Just Female

        _ ->
            Nothing


stringToDosage : String -> Maybe Dosage
stringToDosage str =
    case str of
        "daily_5mg" ->
            Just Daily5mg

        "daily_7mg" ->
            Just Daily7mg

        "divided" ->
            Just Divided

        _ ->
            Nothing



-- Logic


idealBodyWeight : Model -> Maybe Float
idealBodyWeight { sex, height } =
    case height of
        Just height_ ->
            let
                heightInInches =
                    case height_ of
                        Height Inch h ->
                            toFloat h

                        Height Cm h ->
                            toFloat (cmToInches h)

                heightOverFiveFeet =
                    heightInInches - (12 * 5)

                constant =
                    case sex of
                        Male ->
                            50

                        Female ->
                            45.4
            in
            Just (constant + 2.3 * heightOverFiveFeet)

        _ ->
            Nothing



-- Obesity is defined as when a person's weight is at least 20% over their ideal
-- bodyweight


isObese : Model -> Bool
isObese model =
    case ( idealBodyWeight model, model.weight ) of
        ( Just ibw, Just (Weight w) ) ->
            toFloat w > ibw * 1.2

        _ ->
            False


correctedBodyWeight : Model -> Maybe Float
correctedBodyWeight model =
    case ( model.weight, idealBodyWeight model ) of
        ( Just (Weight actual), Just ideal ) ->
            Just (ideal + (0.4 * (toFloat actual - ideal)))

        _ ->
            Nothing


cmToInches : Int -> Int
cmToInches f =
    round (0.393701 * toFloat f)


creatinineClearance : Model -> Maybe Clearance
creatinineClearance { sex, age, weight, serumCreatinine } =
    case ( age, weight, serumCreatinine ) of
        ( Just (Age a), Just (Weight w), Just (SerumCreatinine sc) ) ->
            let
                baseClearance =
                    toFloat ((140 - a) * w) / sc
            in
            case sex of
                Male ->
                    Just (Clearance (baseClearance * 1.23))

                Female ->
                    Just (Clearance (baseClearance * 1.04))

        _ ->
            Nothing



-- A line on (x,y) where x = time between start of infusion and sample draw
--                       y = concentration measured by draw


nomogram24h : Float -> Float
nomogram24h x =
    11.625 - (0.6875 * x)


nomogram36h : Float -> Float
nomogram36h x =
    17 - x


nomogram48h : Float -> Float
nomogram48h x =
    19 - x


type NomogramArea
    = H24
    | H36
    | H48
    | Custom



-- Given an (x,y) point on the nomogram, calculate what area it lies in


nomogramArea : Float -> Float -> NomogramArea
nomogramArea x y =
    if y > nomogram48h x then
        Custom
        -- patient kidney function is too poor - can't use nomogram

    else if y > nomogram36h x then
        H48

    else if y > nomogram24h x then
        H36

    else
        H24



-- Generic Utils


maybeToString : (a -> String) -> Maybe a -> String
maybeToString f s =
    case s of
        Nothing ->
            ""

        Just s_ ->
            f s_



-- Round a Float to the specified number of decimal places


roundFloat : Int -> Float -> Float
roundFloat dp x =
    toFloat (round (x * 100)) / 100
