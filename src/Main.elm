module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Select exposing (fromValuesWithLabels)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Feature =
    { title : String
    , description : String
    , points : Int
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
        -- Scaffolded values to test UI
        [ { title = "faster loading time", description = "right now the first load takes very long", points = 0 }
        , { title = "search for item using itemID too", description = "right now can only search using item name and not item ID", points = 2 }
        ]



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | SortFeatures SortMethod


type SortMethod
    = SortByUnsorted
    | SortByVotesMost
    | SortByVotesLeast
    | SortByTimeLatest
    | SortByTimeOldest


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        SortFeatures sortMethod ->
            { model
                | features =
                    case sortMethod of
                        SortByUnsorted ->
                            model.features

                        SortByVotesMost ->
                            List.reverse (List.sortBy .points model.features)

                        SortByVotesLeast ->
                            List.sortBy .points model.features

                        SortByTimeLatest ->
                            Debug.todo "Not yet implemented"

                        SortByTimeOldest ->
                            Debug.todo "Not yet implemented"
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "All features" ]
        , fromValuesWithLabels
            [ ( SortByUnsorted, "-- Sort --" )
            , ( SortByVotesMost, "Votes (Most)" )
            , ( SortByVotesLeast, "Votes (Least)" )
            , ( SortByTimeLatest, "Time (Latest)" )
            , ( SortByTimeOldest, "Time (Oldest)" )
            ]
            SortByUnsorted
            SortFeatures
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
        [ p [] [ text (toString feature.points ++ " points") ]
        , text feature.title
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
