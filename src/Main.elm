-- TODO: Use Ratio instead of Float
-- TODO: What do we do if patient is less than 5 feet tall?
-- TODO: Display units for creatinine clearance

module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, label, select, option, p,
                      pre, form, br, input)
import Html.Attributes exposing (type_, checked, value, selected, class)
import Html.Events exposing (onClick, onInput, on, targetValue)

import Json.Decode as Json
import Json.Decode exposing (andThen, map)

import Tuple
import List
import Maybe
import String
import String as S exposing (fromInt, fromFloat)

-- Main

main = Browser.sandbox { init = init, update = update, view = view }

-- Model

type alias Model = {
      sex : Sex
    , defaultHeightUnit : HeightUnit
    , height : Maybe Height
    , age : Maybe Age
    , weight : Maybe Weight
    , serumCreatinine : Maybe SerumCreatinine
    , dosage : Dosage
  }

type Sex = Male | Female
type Height = Height HeightUnit Int
type HeightUnit = Cm | Inch
type Weight = Weight Int
type Age = Age Int
type SerumCreatinine = SerumCreatinine Float
type Clearance = Clearance Float
type Dosage = Daily5mg | Daily7mg | Divided

init : Model
init = {
      sex = Female
    , defaultHeightUnit = Inch
    , height = Nothing
    , age = Nothing
    , weight = Nothing
    , serumCreatinine = Nothing
    , dosage = Daily5mg
  }

type Msg = SetSex (Maybe Sex)
         | SetHeightValue HeightUnit String
         | SetHeightUnit String
         | SetWeight String
         | SetAge String
         | SetSerumCreatinine String
         | SetDosage (Maybe Dosage)

update : Msg -> Model -> Model
update msg model = case msg of
  SetSex mSex ->
    case mSex of
      Just s -> { model | sex = s }
      Nothing -> model
  SetHeightValue unit heightStr ->
    { model | height = Maybe.map (Height unit) (String.toInt heightStr) }
  SetHeightUnit unitStr ->
    case stringToHeightUnit unitStr of
      Just u ->
        case model.height of
          Just (Height _ h) -> { model | height = Just (Height u h), defaultHeightUnit = u }
          Nothing -> { model | defaultHeightUnit = u }
      Nothing -> model
  SetWeight weightStr ->
    { model | weight = Maybe.map Weight (String.toInt weightStr) }
  SetAge ageStr ->
    { model | age = Maybe.map Age (String.toInt ageStr) }
  SetSerumCreatinine scStr ->
    { model | serumCreatinine = Maybe.map SerumCreatinine (String.toFloat scStr) }
  SetDosage mDosage ->
    case mDosage of
      Just d -> { model | dosage = d }
      Nothing -> model

view : Model -> Html.Html Msg
view model =
  let obese = case isObese model of
        True -> "Yes"
        False -> "No"
      inputs = [
        sexInput
        , heightInput
        , ageInput
        , weightInput
        , serumCreatinineInput
        , dosageInput
        ]
      clearance = maybeToString S.fromFloat (Maybe.map (\(Clearance c) -> c) (creatinineClearance model))
      correctedWeight = maybeToString S.fromInt (Maybe.map round (correctedBodyWeight model))
      idealWeight = maybeToString S.fromInt (Maybe.map round (idealBodyWeight model))
      weight = if isObese model then (correctedBodyWeight model) else (idealBodyWeight model)
      dailyInitialDose =
        case weight of
          Nothing -> Nothing
          Just w ->
            let mg = List.range 3 5
            in mg |> List.map (\e -> (toFloat e) * w / 3) |> List.maximum
      outputs =
        [
            div [] [ text ("Obese: " ++ obese) ]
          , if isObese model
            then text ("Corrected body weight: " ++ correctedWeight ++ " kg")
            else text ("Ideal body weight: " ++ idealWeight ++ " kg")
          , div [] [text ("Creatinine Clearance: " ++ clearance)]
          , div [] [
              text "Dosage: "
            , dosageInstruction model
            ]
        ]
  in
    div [] [
        div [] [form [] (List.map (\i -> i model) inputs)]
      , br [] []
      , div [] outputs
    ]

dosageInstruction : Model -> Html.Html Msg
dosageInstruction model =
  let weight = if isObese model then correctedBodyWeight else idealBodyWeight
  in case (weight model) of
      Nothing -> div [] []
      Just w ->
        case model.dosage of
            Daily5mg -> daily5mgDosageInstruction model w
            Daily7mg -> daily7mgDosageInstruction model w
            Divided -> dividedDosageInstruction model w

-- Dosage

-- Regimen describes the dosing regimen (how much to give and when)
-- base:      the per-Kg calculated dosage
-- initial:   the actual amount to give as the first dose (base * weight)
-- following: instructions for the next dose
type alias Regimen = { base : Dose, initial : Dose, following : (Hour, DoseInstruction), note : Maybe String }

type alias Hour = Int
type alias Dose = (Int, Int)
type DoseInstruction = Dose | Other String

daily : Model -> Float -> Maybe Regimen
daily model weight =
  case creatinineClearance model of
    Nothing -> Nothing
    Just (Clearance cc) ->
      let
          perKg = if cc < 20.0 then (2, 3) else (5, 7)
          note = if cc < 20.0 then Just "poor kidney function" else Nothing
          (min, max) = let f = (\x -> round (x * weight)) in Tuple.mapBoth f f perKg
          following =
            if cc >= 60.0 then
              (24, Dose)
            else if cc >= 40.0 then
              (36, Dose)
            else if cc >= 20.0 then
              (48, Dose)
            else
              (48, Other "take gentamicin levels; apply next dose when levels fall to < 1 µmol/L")
      in
          Just { base = perKg, initial = (min, max), following = following, note = note}

daily5mgDosageInstruction : Model -> Float -> Html.Html Msg
daily5mgDosageInstruction model weight =
  case daily model weight of
    Nothing -> div [] []
    Just { base, initial, following, note } ->
      let
        initialStr = let (min, _) = initial in (S.fromInt min) ++ "mg"
        rangeStr (a, b) = (S.fromInt a) ++ "-" ++ (S.fromInt b) ++ " mg"
        followingStr = case following of
          (hour, Dose) -> "Dose every " ++ S.fromInt hour ++ " hours"
          (hour, Other s) -> "At " ++ S.fromInt hour ++ " hours: " ++ s
      in
          div [] [
              div [] [text ("Initial dose: " ++ initialStr)]
            , case note of
                Nothing -> div [] []
                Just n -> div [] [text ("Note: " ++ n)]
            , div [] [text followingStr]
          ]

-- TODO
-- if using 7 mg/kg daily dose (hartford nomogram):
-- take gentamicin level 6-14 hours after first dose
-- compare level with hartford nomogram to determine next dosage interval
daily7mgDosageInstruction : Model -> Float -> Html.Html Msg
daily7mgDosageInstruction model weight =
  case daily model weight of
    Nothing -> div [] []
    Just { base, initial } ->
      let
        initialStr = let (_, max) = initial in (S.fromInt max) ++ "mg"
      in
        div [] [
          div [] [text ("Initial dose: " ++ initialStr)]
          , div [] [text ("Take gentamicin levels 6-14 hours after first dose")]
          , div [] [text ("Compare level with Hartford nomogram to determine next dosage interval")]
        ]

dividedDosageInstruction : Model -> Float -> Html.Html Msg
dividedDosageInstruction model weight =
  let
      (min, max) = (3 * weight / 3, 5 * weight / 3)
      range =
        (min |> round |> S.fromInt)
        ++ "-"
        ++ (max |> round |> S.fromInt)
        ++ " mg"
  in
      p [] [
          div [] [text (range ++ " every 8 hours (from base dose of 3-5 mg/kg)") ]
        , div [] [text "After 1 hour, gentamicin levels should be 5-10 mg/L"]
        , div [] [text "After 24 hours, gentamicin levels should be < 2 mg/L"]
      ]

-- Inputs

sexInput : Model -> Html.Html Msg
sexInput { sex } =
  div [] [
      label [] [ text "Sex: " ]
    , select [ on "change" (map (SetSex << stringToSex) targetValue) ] [
          option [ value "male", selected (sex == Male)] [ text "Male" ]
        , option [ value "female", selected (sex == Female) ] [ text "Female" ]
      ]
  ]

heightInput : Model -> Html.Html Msg
heightInput { height, defaultHeightUnit } =
  let
      unit = Maybe.withDefault defaultHeightUnit (Maybe.map (\(Height u _) -> u) height)
      h = Maybe.map (\(Height _ h_) -> h_) height
  in
    div [] [
        label [] [ text "Height: " ]
      , input [ type_ "number", value (maybeToString S.fromInt h), onInput (SetHeightValue unit) ] []
      , select [ on "change" (map SetHeightUnit targetValue) ] [
            option [ selected (unit == Cm), value "cm" ] [ text "Centimetres" ]
          , option [ selected (unit == Inch), value "inch" ] [ text "Inches" ]
        ]
      , text (Maybe.withDefault "" (Maybe.map (\s -> " (" ++ s ++ ")") (Maybe.map heightToString height)))
    ]

weightInput : Model -> Html.Html Msg
weightInput { weight } =
  let weightStr =
        Maybe.withDefault "" (Maybe.map (\(Weight w) -> S.fromInt w) weight)
  in
    div [] [
      label [] [ text "Weight: " ]
      , input [ type_ "number", value weightStr, onInput SetWeight ] []
      , text " kg"
    ]

ageInput : Model -> Html.Html Msg
ageInput { age } =
  let ageStr = Maybe.withDefault "" (Maybe.map (\(Age a) -> S.fromInt a) age)
  in
    div [] [
        label [] [ text "Age: " ]
      , input [ type_ "number", value ageStr, onInput SetAge ] []
      , text " years"
      ]

serumCreatinineInput : Model -> Html.Html Msg
serumCreatinineInput { serumCreatinine } =
  let scStr = Maybe.withDefault "" (Maybe.map (\(SerumCreatinine sc) -> S.fromFloat sc) serumCreatinine)
  in
    div [] [
        label [] [ text "Serum Creatinine: " ]
      , input [ type_ "number", value scStr, onInput SetSerumCreatinine ] []
      , text " µmol/L"
      ]

dosageInput : Model -> Html.Html Msg
dosageInput { dosage } =
  div [] [
      label [] [ text "Dosage: " ]
    , select [ on "change" (map (SetDosage << stringToDosage) targetValue) ] [
          option [ value "daily_5mg", selected (dosage == Daily5mg) ] [ text "Daily (5 mg)" ]
        , option [ value "daily_7mg", selected (dosage == Daily7mg)] [ text "Daily (7 mg)" ]
        , option [ value "divided", selected (dosage == Divided) ] [ text "Divided" ]
      ]
  ]

-- Specific Utils

heightToString : Height -> String
heightToString (Height unit h) =
  case unit of
    Cm -> cmToString h
    Inch -> inchToString h

inchToString : Int -> String
inchToString h =
  let feet = h // 12
      inches = remainderBy 12 h
  in
      if feet == 0
      then (S.fromInt h) ++ " inches"
      else (S.fromInt feet) ++ " feet " ++ (S.fromInt inches) ++ " inches"

cmToString : Int -> String
cmToString h =
  let metres = h // 100
      cm = remainderBy 100 h
  in
      if metres == 0
      then (S.fromInt h) ++ " cm"
      else (S.fromInt metres) ++ "m " ++ (S.fromInt cm) ++ "cm"

stringToHeightUnit : String -> Maybe HeightUnit
stringToHeightUnit str = case str of
  "cm" -> Just Cm
  "inch" -> Just Inch
  _ -> Nothing

stringToSex : String -> Maybe Sex
stringToSex str = case str of
  "male" -> Just Male
  "female" -> Just Female
  _ -> Nothing

stringToDosage : String -> Maybe Dosage
stringToDosage str = case str of
  "daily_5mg" -> Just Daily5mg
  "daily_7mg" -> Just Daily7mg
  "divided" -> Just Divided
  _ -> Nothing

-- Logic

idealBodyWeight : Model -> Maybe Float
idealBodyWeight { sex, height } =
  case height of
    Just height_ ->
      let heightInInches = case height_ of
                            Height Inch h -> toFloat h
                            Height Cm h -> toFloat (cmToInches h)
          heightOverFiveFeet = heightInInches - (12 * 5)
          constant = case sex of
                       Male -> 50
                       Female -> 45.4
      in Just (constant + 2.3 * heightOverFiveFeet)
    _ -> Nothing

-- Obesity is defined as when a person's weight is at least 20% over their ideal
-- bodyweight
isObese : Model -> Bool
isObese model =
  case (idealBodyWeight model, model.weight) of
    (Just ibw, Just (Weight w)) -> toFloat w > ibw * 1.2
    _ -> False

correctedBodyWeight : Model -> Maybe Float
correctedBodyWeight model =
  case (model.weight, idealBodyWeight model) of
    (Just (Weight actual), Just ideal) ->
      Just (ideal + (0.4 * ((toFloat actual) - ideal)))
    _ -> Nothing

cmToInches : Int -> Int
cmToInches f = round (0.393701 * (toFloat f))

creatinineClearance : Model -> Maybe Clearance
creatinineClearance { sex, age, weight, serumCreatinine } =
  case (age, weight, serumCreatinine) of
    (Just (Age a), Just (Weight w), Just (SerumCreatinine sc)) ->
      let baseClearance = toFloat ((140 - a) * w) / sc
      in
        case sex of 
          Male -> Just (Clearance (baseClearance * 1.23))
          Female -> Just (Clearance (baseClearance * 1.04))
    _ -> Nothing

-- Generic Utils

maybeToString : (a -> String) -> Maybe a -> String
maybeToString f s =
  case s of
    Nothing -> ""
    Just s_ -> f s_
