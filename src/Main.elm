-- TODO: Use Ratio instead of Float
-- TODO: What do we do if patient is less than 5 feet tall?
-- TODO: Display units for creatinine clearance
-- TOOD: calc BMI and obesity from height and weight

module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input, label, select, option, p,
                      pre, form, br)
import Html.Attributes exposing (type_, checked, value, selected, class)
import Html.Events exposing (onClick, onInput, on, targetValue)

import Json.Decode as Json
import Json.Decode exposing (andThen, map)

import Tuple
import List
import Maybe
import String
import Debug exposing (toString)

-- Main

main = Browser.sandbox { init = init, update = update, view = view }

-- Model

type alias Model = {
      isObese : Bool
    , sex : Sex
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
type Dosage = Daily | Divided

init : Model
init = {
    isObese = False
    , sex = Female
    , defaultHeightUnit = Inch
    , height = Nothing
    , age = Nothing
    , weight = Nothing
    , serumCreatinine = Nothing
    , dosage = Daily
  }

type Msg = ToggleObese
         | SetSex (Maybe Sex)
         | SetHeightValue HeightUnit String
         | SetHeightUnit String
         | SetWeight String
         | SetAge String
         | SetSerumCreatinine String
         | SetDosage (Maybe Dosage)

update : Msg -> Model -> Model
update msg model = case msg of
  ToggleObese ->
    { model | isObese = not model.isObese }
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
  let obese = case model.isObese of
        True -> "Yes"
        False -> "No"
      inputs = [
        sexInput
        , heightInput
        , ageInput
        , obeseInput
        , weightInput
        , serumCreatinineInput
        , dosageInput
        ]
      clearance = maybeToString (Maybe.map (\(Clearance c) -> c) (creatinineClearance model))
      correctedWeight = maybeToString (Maybe.map round (correctedBodyWeight model))
      idealWeight = maybeToString (Maybe.map round (idealBodyWeight model))
      weight = if model.isObese then (correctedBodyWeight model) else (idealBodyWeight model)
      dailyInitialDose =
        case weight of
          Nothing -> Nothing
          Just w ->
            let mg = List.range 3 5
            in mg |> List.map (\e -> (toFloat e) * w / 3) |> List.maximum
      outputs =
        [
            if model.isObese
            then text ("Corrected body weight: " ++ correctedWeight ++ " kg")
            else text ("Ideal body weight: " ++ idealWeight ++ " kg")
          , div [] [text ("Creatinine Clearance: " ++ clearance)]
          , div [] [
              text "Dosage: "
            , dosageInstruction model
            ]
        ]
  in
    div [class "container"] [
        div [] [form [] (List.map (\i -> i model) inputs)]
      , br [] []
      , div [] outputs
    ]

dosageInstruction : Model -> Html.Html Msg
dosageInstruction model =
  let weight = if model.isObese then correctedBodyWeight else idealBodyWeight
  in case (weight model) of
      Nothing -> div [] []
      Just w ->
        case model.dosage of
            Daily -> dailyDosageInstruction model w
            Divided -> dividedDosageInstruction model w

-- Regimen describes the dosing regimen (how much to give and when)
-- base:      the per-Kg calculated dosage
-- initial:   the actual amount to give as the first dose (base * weight)
-- following: instructions for the next dose
type alias Regimen = { base : Dose, initial : Dose, following : (Hour, DoseInstruction), note : Maybe String }

type alias Hour = Int
type alias Dose = (Int, Int)
type DoseInstruction = Dose | Other String

-- TODO
-- if using 7 mg/kg daily dose (hartford nomogram):
-- take gentamicin level 6-14 hours after first dose
-- compare level with hartford nomogram to determine next dosage interval
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

dailyDosageInstruction : Model -> Float -> Html.Html Msg
dailyDosageInstruction model weight =
  case daily model weight of
    Nothing -> div [] []
    Just { base, initial, following, note } ->
      let
        rangeStr (a, b) = (toString a) ++ "-" ++ (toString b) ++ " mg"
        followingStr = case following of
          (hour, Dose) -> "Dose every " ++ toString hour ++ " hours"
          (hour, Other s) -> "At " ++ toString hour ++ " hours: " ++ s
      in
          div [] [
              div [] [text ("Initial dose: " ++ rangeStr initial ++ " (from base dose of " ++ rangeStr base ++ ")")]
            , case note of
                Nothing -> div [] []
                Just n -> div [] [text ("Note: " ++ n)]
            , div [] [text followingStr]
          ]

dividedDosageInstruction : Model -> Float -> Html.Html Msg
dividedDosageInstruction model weight =
  let
      (min, max) = (3 * weight / 3, 5 * weight / 3)
      range =
        (min |> round |> toString)
        ++ "-"
        ++ (max |> round |> toString)
        ++ " mg"
  in
      div [] [
        p [] [text (range ++ " every 8 hours (from base dose of 3-5 mg/kg)") ]
        , p [] [text "After 1 hour, gentamicin levels should be 5-10 mg/L"]
        , p [] [text "After 24 hours, gentamicin levels should be < 2 mg/L"]
      ]

-- Inputs

obeseInput : Model -> Html.Html Msg
obeseInput model = div [] [
    label [] [ text "Is this person obese? " ]
  , input [ type_ "checkbox", checked model.isObese, onClick ToggleObese ] []
  ]

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
      , input [ type_ "number", value (maybeToString h), onInput (SetHeightValue unit) ] []
      , select [ on "change" (map SetHeightUnit targetValue) ] [
            option [ selected (unit == Cm), value "cm" ] [ text "Centimetres" ]
          , option [ selected (unit == Inch), value "inch" ] [ text "Inches" ]
        ]
      , text (" (" ++ heightToString height ++ ")")
    ]

weightInput : Model -> Html.Html Msg
weightInput { weight } =
  let weightStr =
        Maybe.withDefault "" (Maybe.map (\(Weight w) -> toString w) weight)
  in
    div [] [
      label [] [ text "Weight: " ]
      , input [ type_ "number", value weightStr, onInput SetWeight ] []
      , text " kg"
    ]

ageInput : Model -> Html.Html Msg
ageInput { age } =
  let ageStr = Maybe.withDefault "" (Maybe.map (\(Age a) -> toString a) age)
  in
    div [] [
        label [] [ text "Age: " ]
      , input [ type_ "number", value ageStr, onInput SetAge ] []
      , text " years"
      ]

serumCreatinineInput : Model -> Html.Html Msg
serumCreatinineInput { serumCreatinine } =
  let scStr = Maybe.withDefault "" (Maybe.map (\(SerumCreatinine sc) -> toString sc) serumCreatinine)
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
          option [ value "daily", selected (dosage == Daily)] [ text "Daily" ]
        , option [ value "divided", selected (dosage == Divided) ] [ text "Divided" ]
      ]
  ]

-- Specific Utils

heightToString : Maybe Height -> String
heightToString height =
  case height of
    Nothing -> ""
    Just (Height unit h) ->
      case unit of
        Cm -> cmToString h
        Inch -> inchToString h

inchToString : Int -> String
inchToString h =
  let feet = h // 12
      inches = remainderBy 12 h
  in
      if feet == 0
      then (toString h) ++ " inches"
      else (toString feet) ++ " feet " ++ (toString inches) ++ " inches"

cmToString : Int -> String
cmToString h =
  let metres = h // 100
      cm = remainderBy 100 h
  in
      if metres == 0
      then (toString h) ++ " cm"
      else (toString metres) ++ "m " ++ (toString cm) ++ "cm"

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
  "daily" -> Just Daily
  "divided" -> Just Divided
  _ -> Nothing

-- Logic

idealBodyWeight : Model -> Maybe Float
idealBodyWeight { isObese, sex, height } =
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

maybeToString : Maybe a -> String
maybeToString s =
  case s of
    Nothing -> ""
    Just s_ -> toString s_
