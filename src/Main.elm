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
    { newFeature : Feature
    , features : List Feature
    }


init : Model
init =
    { newFeature = { title = "", description = "", points = 0 }
    , -- Scaffolded values to test UI
      features =
        [ { title = "faster loading time", description = "right now the first load takes very long", points = 0 }
        , { title = "search for item using itemID too", description = "right now can only search using item name and not item ID", points = 2 }
        ]
    }



-- UPDATE


type Msg
    = -- Sort the features by a given sort method
      SortFeatures SortMethod
      -- Set values of input into model for new feature
    | SetTitle String
    | SetDescription String
    | SetPoints Int
      -- Create a new feature and append into model
    | NewFeature


type SortMethod
    = SortByUnsorted
    | SortByVotesMost
    | SortByVotesLeast
    | SortByTimeLatest
    | SortByTimeOldest


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTitle title ->
            let
                newFeature_ =
                    model.newFeature

                newFeature =
                    { newFeature_ | title = title }
            in
            { model | newFeature = newFeature }

        SetDescription description ->
            let
                newFeature_ =
                    model.newFeature

                newFeature =
                    { newFeature_ | description = description }
            in
            { model | newFeature = newFeature }

        SetPoints points ->
            let
                newFeature_ =
                    model.newFeature

                newFeature =
                    { newFeature_ | points = points }
            in
            { model | newFeature = newFeature }

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

        NewFeature ->
            { model | features = model.newFeature :: model.features }



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
        , viewInput "text" "Title" model.newFeature.title SetTitle
        , viewInput "text" "Description" model.newFeature.description SetDescription
        , button [ Html.Events.onClick NewFeature ] [ text "new" ]
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
