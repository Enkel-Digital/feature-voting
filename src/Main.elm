module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Feature =
    { title : String
    , description : String
    }


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , features : List Feature
    }


init : Model
init =
    Model ""
        ""
        ""
        [ { title = "faster loading time", description = "right now the first load takes very long" }
        , { title = "search for item using itemID too", description = "right now can only search using item name and not item ID" }
        ]



-- Scaffolded values to test UI
-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "All features" ]
        , viewFeatures model.features
        , viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewFeatures : List Feature -> Html msg
viewFeatures features =
    ul []
        (List.map (\feature -> li [] [ viewFeature feature ]) features)


viewFeature : Feature -> Html msg
viewFeature feature =
    div []
        [ text feature.title
        , hr [] []
        , text feature.description
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if model.password == model.passwordAgain then
        div [ style "color" "green" ] [ text "OK" ]

    else
        div [ style "color" "red" ] [ text "Passwords do not match!" ]
