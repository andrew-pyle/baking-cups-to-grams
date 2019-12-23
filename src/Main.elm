module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Round



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { cupsInputWhole : Int
    , cupsInputFraction : Fraction
    }


type alias Fraction =
    { numerator : Int
    , denominator : Int
    }


init : Model
init =
    Model 1 { numerator = 1, denominator = 4 }


type BakingIngredient
    = Flour
    | GranulatedSugar
    | BrownSugar
    | Butter
    | Shortening
    | PowderedSugar
    | PeanutButter


bakingIngredients : List BakingIngredient
bakingIngredients =
    [ Flour
    , GranulatedSugar
    , BrownSugar
    , Butter
    , Shortening
    , PowderedSugar
    , PeanutButter
    ]



-- UPDATE


type Msg
    = Increment Int
    | Decrement Int
    | AlterFraction Fraction
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment int ->
            { model | cupsInputWhole = model.cupsInputWhole + int }

        Decrement fraction ->
            if model.cupsInputWhole > 0 then
                { model | cupsInputWhole = model.cupsInputWhole - fraction }

            else
                model

        AlterFraction fraction ->
            { model | cupsInputFraction = fraction }

        Clear ->
            { model | cupsInputWhole = 0, cupsInputFraction = Fraction 0 1 }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 []
            [ text "Cups "
            , span [ class "unicode-arrow" ] [ text "→" ]
            , text " Grams of Ingredient"
            ]
        , output [ class "input-display" ] [ modelToHtml model ]
        , Html.form []
            [ button [ class "btn one", type_ "button", Html.Events.onClick <| Increment 1 ]
                [ text "+ 1"
                ]
            , button [ class "btn one", type_ "button", Html.Events.onClick <| Decrement 1 ]
                [ text "- 1"
                ]
            , button [ class "btn one-quarter", type_ "button", Html.Events.onClick <| AlterFraction <| Fraction 1 4 ]
                [ text "¼"
                ]
            , button [ class "btn one-third", type_ "button", Html.Events.onClick <| AlterFraction <| Fraction 1 3 ]
                [ text "⅓"
                ]
            , button [ class "btn one-half", type_ "button", Html.Events.onClick <| AlterFraction <| Fraction 1 2 ]
                [ text "½"
                ]
            , button [ class "btn two-thirds", type_ "button", Html.Events.onClick <| AlterFraction <| Fraction 2 3 ]
                [ text "⅔"
                ]
            , button [ class "btn three-fourths", type_ "button", Html.Events.onClick <| AlterFraction <| Fraction 3 4 ]
                [ text "¾"
                ]
            , button [ class "btn clear", type_ "button", Html.Events.onClick Clear ] [ text "⌫" ]
            ]

        --     [ input
        --         [ id "input-cups"
        --         , class "input-custom"
        --         , type_ "number"
        --         , pattern "[0-9.]*"
        --         , value model.cupsInputWhole
        --         , onInput CupsInput
        --         ]
        --         []
        --     , label [ for "input-cups" ] [ text "Cups" ]
        , table
            [ class "grams-output" ]
            [ thead []
                []
            , tbody
                []
                (List.map
                    (\ingredient ->
                        tr []
                            [ td [ class "td-ingredient" ] [ text <| bakingIngredientToString ingredient ]
                            , td [ class "td-gram-value" ]
                                [ text <| Round.round 7 <| modelToValue model ]

                            -- [ text <| Round.round 0 <| modelToValue model ]
                            , td [ class "td-unit" ] [ text "g" ]
                            ]
                    )
                    bakingIngredients
                )
            ]
        ]



-- HELPERS


maybeCalculateGrams : String -> BakingIngredient -> String
maybeCalculateGrams numberOfCups ingredient =
    case String.toFloat numberOfCups of
        Just value ->
            Round.round 0 <| cupsToGrams value ingredient

        Nothing ->
            ""


cupsToGrams : Float -> BakingIngredient -> Float
cupsToGrams cups unit =
    case unit of
        Flour ->
            cups * 128.0

        GranulatedSugar ->
            cups * 200.0

        BrownSugar ->
            cups * 200.0

        Butter ->
            cups * 230.0

        PeanutButter ->
            cups * (213.0 / 1.5)

        Shortening ->
            cups * 205.0

        PowderedSugar ->
            cups * 125.0


bakingIngredientToString : BakingIngredient -> String
bakingIngredientToString ingredient =
    case ingredient of
        Flour ->
            "Flour"

        GranulatedSugar ->
            "Granulated Sugar"

        BrownSugar ->
            "Brown Sugar"

        Butter ->
            "Butter"

        Shortening ->
            "Shortening"

        PowderedSugar ->
            "Powdered Sugar"

        PeanutButter ->
            "Peanut Butter"


modelToHtml : Model -> Html msg
modelToHtml model =
    div []
        [ div [ class "whole-number" ] [ text <| String.fromInt <| model.cupsInputWhole ]
        , div [ class "fraction" ]
            [ div [ class "fraction-part numerator" ] [ text <| String.fromInt <| model.cupsInputFraction.numerator ]
            , div [ class "fraction-divider" ] []
            , div [ class "fraction-part denomerator" ]
                [ text <|
                    String.fromInt <|
                        if model.cupsInputFraction.denominator == 1 then
                            (Debug.log <| Debug.toString model.cupsInputFraction.denominator)
                                0

                        else
                            (Debug.log <| Debug.toString model.cupsInputFraction.denominator)
                                model.cupsInputFraction.denominator
                ]
            ]
        ]


modelToValue : Model -> Float
modelToValue model =
    toFloat model.cupsInputWhole
        + toFloat model.cupsInputFraction.numerator
        / toFloat model.cupsInputFraction.denominator
