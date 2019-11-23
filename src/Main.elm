-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--


module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Round



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { numberOfCups : Float }


init : Model
init =
    Model 1.0


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
    = Increment
    | Decrement
    | CupsInput (Maybe Float)


update : Msg -> Model -> Model
update msg model =
    -- TODO Fix issues: can't erase the value when typing
    case msg of
        Increment ->
            { model | numberOfCups = model.numberOfCups + 1.0 }

        Decrement ->
            if model.numberOfCups > 1.0 then
                { model | numberOfCups = model.numberOfCups - 1.0 }

            else
                model

        CupsInput input ->
            case input of
                Just value ->
                    if value > 0 then
                        { model | numberOfCups = value }

                    else
                        model

                Nothing ->
                    model



-- VIEW


view : Model -> Html Msg
view model =
    -- TODO Map out this component
    -- TODO Use float for cups input & computation
    div [ class "container" ]
        [ h1 []
            [ text "Cups "
            , span [ class "unicode-arrow" ] [ text "→" ]
            , text " Grams of Ingredient"
            ]
        , button [ class "btn btn-round", onClick Decrement ] [ text "–" ]
        , input
            [ class "input-cups"
            , placeholder "Cups"
            , value <| String.fromFloat <| model.numberOfCups
            , onInput parseCupsInput
            ]
            []
        , button [ class "btn btn-round", onClick Increment ] [ text "+" ]
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
                                [ text <| Round.round 2 <| cupsToGrams model.numberOfCups ingredient ]
                            , td [ class "td-unit" ] [ text "g" ]
                            ]
                    )
                    bakingIngredients
                )
            ]
        ]



-- HELPERS


parseCupsInput : String -> Msg
parseCupsInput inputText =
    CupsInput (String.toFloat inputText)


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
