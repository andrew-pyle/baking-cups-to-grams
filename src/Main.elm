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



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { numberOfCups : Int }



--{ name : String
--, password : String
--, passwordAgain : String
--}


init : Model
init =
    Model 1



-- Model "" "" ""
-- UPDATE


type Msg
    = Increment
    | Decrement
    | CupsInput (Maybe Int)



--= Name String
--| Password String
--| PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    -- TODO Fix issues: not DRY logic, can't erase the value when typing
    case msg of
        Increment ->
            if model.numberOfCups > 0 then
                { model | numberOfCups = model.numberOfCups + 1 }

            else
                model

        Decrement ->
            if model.numberOfCups > 1 then
                { model | numberOfCups = model.numberOfCups - 1 }

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



--Name name ->
--  { model | name = name }
--Password password ->
--  { model | password = password }
--PasswordAgain password ->
--  { model | passwordAgain = password }
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
            , value (String.fromInt model.numberOfCups)
            , onInput parseCupsInput
            ]
            []
        , button [ class "btn btn-round", onClick Increment ] [ text "+" ]

        -- , div []
        --     [
        --     ul []
        --         [ li []
        --             [ text <|
        --                 String.append "Flour: " <|
        --                     String.fromInt <|
        --                         cupsToGrams model.numberOfCups Flour
        --             ]
        --         , li []
        --             [ cupsToGrams
        --                 model.numberOfCups
        --                 GranulatedSugar
        --                 |> String.fromInt
        --                 |> String.append "Sugar: "
        --                 |> text
        --             ]
        --         , li []
        --             [ text <|
        --                 "Brown Sugar: "
        --                     ++ String.fromInt (cupsToGrams model.numberOfCups BrownSugar)
        --             ]
        --         , li []
        --             [ text
        --                 ("Butter: "
        --                     ++ String.fromInt (cupsToGrams model.numberOfCups Butter)
        --                 )
        --             ]
        --         ]
        --     ]
        , table
            [ class "grams-output" ]
            [ thead []
                []
            , tbody
                []
                [ tr []
                    [ td [ class "td-ingredient" ] [ text "Flour" ]
                    , td [ class "td-gram-value" ] [ text <| String.fromInt (cupsToGrams model.numberOfCups Flour) ]
                    , td [ class "td-unit" ] [ text "g" ]
                    ]
                , tr []
                    [ td [] [ text "Granulated Sugar" ]
                    , td [] [ text <| String.fromInt (cupsToGrams model.numberOfCups GranulatedSugar) ]
                    , td [] [ text "g" ]
                    ]
                , tr []
                    [ td []
                        [ text "Brown Sugar" ]
                    , td [] [ text <| String.fromInt (cupsToGrams model.numberOfCups BrownSugar) ]
                    , td [] [ text "g" ]
                    ]
                , tr []
                    [ td []
                        [ text "Butter" ]
                    , td
                        []
                        [ text <| String.fromInt (cupsToGrams model.numberOfCups Butter) ]
                    , td [] [ text "g" ]
                    ]
                , tr []
                    [ td []
                        [ text "Shortening" ]
                    , td
                        []
                        [ text <| String.fromInt (cupsToGrams model.numberOfCups Shortening) ]
                    , td [] [ text "g" ]
                    ]
                , tr []
                    [ td []
                        [ text "Powdered Sugar" ]
                    , td
                        []
                        [ text <| String.fromInt (cupsToGrams model.numberOfCups PowderedSugar) ]
                    , td [] [ text "g" ]
                    ]
                , tr []
                    [ td []
                        [ text "Peanut Butter" ]
                    , td
                        []
                        [ text <| String.fromInt (cupsToGrams model.numberOfCups PeanutButter) ]
                    , td [] [ text "g" ]
                    ]
                ]
            ]

        -- , p [] [text <| String.fromInt model.numberOfCups]
        --, viewInput "text" "Name" model.name Name
        --, viewInput "password" "Password" model.password Password
        --, viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        --, viewValidation model
        ]


parseCupsInput : String -> Msg
parseCupsInput inputText =
    CupsInput (String.toInt inputText)


type BakingIngredient
    = Flour
    | GranulatedSugar
    | BrownSugar
    | Butter
    | Shortening
    | PowderedSugar
    | PeanutButter


cupsToGrams : Int -> BakingIngredient -> Int
cupsToGrams cups unit =
    case unit of
        Flour ->
            cups * 128

        GranulatedSugar ->
            cups * 200

        BrownSugar ->
            cups * 200

        Butter ->
            cups * 230

        PeanutButter ->
            cups * round (213 / 1.5)

        Shortening ->
            cups * 205

        PowderedSugar ->
            cups * 125



--viewInput : String -> String -> String -> (String -> msg) -> Html msg
--viewInput t p v toMsg =
--  input [ type_ t, placeholder p, value v, onInput toMsg ] []
--viewValidation : Model -> Html msg
--viewValidation model =
--  if model.password == model.passwordAgain then
--    div [ style "color" "green" ] [ text "OK" ]
--  else
--    div [ style "color" "red" ] [ text "Passwords do not match!" ]
