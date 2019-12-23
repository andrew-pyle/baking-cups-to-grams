module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Round



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { cupsInputWhole : Int
    , cupsInputFraction : Frac
    }



-- type alias Fraction =
--     { numerator : Int
--     , denominator : Int
--     }


type Frac
    = OneFourth
    | OneThird
    | OneHalf
    | TwoThirds
    | ThreeFourths
    | Nothing


init : Model
init =
    Model 1 Nothing


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
    | AlterFraction Frac
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
            { model | cupsInputWhole = 0, cupsInputFraction = Nothing }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 []
            [ text "Cups "
            , span [ class "unicode-arrow" ] [ text "→" ]
            , text " Grams of Ingredient"
            ]
        , Html.form []
            [ output [ class "input-display" ] [ text <| String.fromInt model.cupsInputWhole ++ fractionToString model.cupsInputFraction ]
            , button [ class "btn one", type_ "button", onClick <| Increment 1 ]
                [ text "+ 1"
                ]
            , button [ class "btn one", type_ "button", onClick <| Decrement 1 ]
                [ text "- 1"
                ]
            , button [ class "btn one-quarter", type_ "button", onClick <| AlterFraction OneFourth ]
                [ text "¼"
                ]
            , button [ class "btn one-third", type_ "button", onClick <| AlterFraction OneThird ]
                [ text "⅓"
                ]
            , button [ class "btn one-half", type_ "button", onClick <| AlterFraction OneHalf ]
                [ text "½"
                ]
            , button [ class "btn two-thirds", type_ "button", onClick <| AlterFraction TwoThirds ]
                [ text "⅔"
                ]
            , button [ class "btn three-fourths", type_ "button", onClick <| AlterFraction ThreeFourths ]
                [ text "¾"
                ]
            , button [ class "btn clear", type_ "button", onClick Clear ] [ text "⌫" ]
            ]
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
                                [ text <| Round.round 0 <| cupsToGrams (modelToValue model) ingredient ]

                            -- [ text <| Round.round 0 <| modelToValue model ]
                            , td [ class "td-unit" ] [ text "g" ]
                            ]
                    )
                    bakingIngredients
                )
            ]
        ]



-- HELPERS


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
    p [ class "mixed-number" ]
        [ text <| String.fromInt model.cupsInputWhole ++ fractionToString model.cupsInputFraction ]


modelToValue : Model -> Float
modelToValue model =
    toFloat model.cupsInputWhole + fractionToFloat model.cupsInputFraction


fractionToString : Frac -> String
fractionToString fraction =
    case fraction of
        OneFourth ->
            "¼"

        OneThird ->
            "⅓"

        OneHalf ->
            "½"

        TwoThirds ->
            "⅔"

        ThreeFourths ->
            "¾"

        Nothing ->
            ""


fractionToFloat : Frac -> Float
fractionToFloat fraction =
    case fraction of
        OneFourth ->
            1 / 4

        OneThird ->
            1 / 3

        OneHalf ->
            1 / 2

        TwoThirds ->
            2 / 3

        ThreeFourths ->
            3 / 4

        Nothing ->
            0
