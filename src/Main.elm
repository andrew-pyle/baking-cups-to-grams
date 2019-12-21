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
    { numberOfCups : String }


init : Model
init =
    Model ""


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



-- UPDATE


type Msg
    = CupsInput String



-- | Increment
-- | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- Increment ->
        --     { model | numberOfCups = model.numberOfCups + 1.0 }
        -- Decrement ->
        --     if model.numberOfCups > 1.0 then
        --         { model | numberOfCups = model.numberOfCups - 1.0 }
        --     else
        --         model
        CupsInput input ->
            { model | numberOfCups = input }



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
            [ input
                [ id "input-cups"
                , class "input-custom"
                , type_ "number"
                , pattern "[0-9.]*"
                , value model.numberOfCups
                , onInput CupsInput
                ]
                []
            , label [ for "input-cups" ] [ text "Cups" ]
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
                                [ text <| maybeCalculateGrams model.numberOfCups ingredient ]
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
