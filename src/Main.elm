module Main exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Select exposing (fromValuesWithLabels)
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Feature =
    { title : String
    , description : String
    , points : Int
    , createdAt : Time.Posix
    }


type alias Model =
    { newFeature : Feature
    , features : List Feature
    , time : Maybe Time.Posix
    }


defaultNewFeature : Feature
defaultNewFeature =
    { title = "", description = "", points = 0, createdAt = Time.millisToPosix 0 }


init : Model
init =
    { newFeature = defaultNewFeature
    , -- Scaffolded values to test UI
      features =
        [ { title = "faster loading time", description = "right now the first load takes very long", points = 0, createdAt = Time.millisToPosix 1627985070000 }
        , { title = "search for item using itemID too", description = "right now can only search using item name and not item ID", points = 2, createdAt = Time.millisToPosix 1627975070000 }
        ]
    , time = Nothing
    }



-- UPDATE


type Msg
    = -- Sort the features by a given sort method
      SortFeatures SortMethod
      -- Set values of input into model for new feature
    | SetTitle String
    | SetDescription String
    | SetPoints Int
      -- Create a new feature and append into model before clearing the new feature input form
    | NewFeature
    | NewTime Time.Posix
    | GetCurrentTime


type SortMethod
    = SortByUnsorted
    | SortByVotesMost
    | SortByVotesLeast
    | SortByTimeLatest
    | SortByTimeOldest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTitle title ->
            let
                newFeature_ =
                    model.newFeature

                newFeature =
                    { newFeature_ | title = title }
            in
            ( { model | newFeature = newFeature }, Cmd.none )

        SetDescription description ->
            let
                newFeature_ =
                    model.newFeature

                newFeature =
                    { newFeature_ | description = description }
            in
            ( { model | newFeature = newFeature }, Cmd.none )

        SetPoints points ->
            let
                newFeature_ =
                    model.newFeature

                newFeature =
                    { newFeature_ | points = points }
            in
            ( { model | newFeature = newFeature }, Cmd.none )

        SortFeatures sortMethod ->
            ( { model
                | features =
                    case sortMethod of
                        SortByUnsorted ->
                            model.features

                        SortByVotesMost ->
                            List.reverse (List.sortBy .points model.features)

                        SortByVotesLeast ->
                            List.sortBy .points model.features

                        SortByTimeLatest ->
                            List.reverse (List.sortBy (\feature -> Time.posixToMillis feature.createdAt) model.features)

                        SortByTimeOldest ->
                            List.sortBy (\feature -> Time.posixToMillis feature.createdAt) model.features
              }
            , Cmd.none
            )

        NewFeature ->
            ( { model | features = model.newFeature :: model.features, newFeature = defaultNewFeature }, Cmd.none )

        NewTime time ->
            ( { model | time = Just time }, Cmd.none )

        GetCurrentTime ->
            ( model, Task.perform NewTime Time.now )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
        , button [ Html.Events.onClick GetCurrentTime ] [ Html.text "Get now time." ]
        , case model.time of
            Nothing ->
                Html.text "Empty message."

            Just t ->
                Html.text (toString t)
        ]


viewFeatures : List Feature -> Html msg
viewFeatures features =
    ul []
        (List.map (\feature -> li [] [ viewFeature feature ]) features)


viewFeature : Feature -> Html msg
viewFeature feature =
    div []
        [ p [] [ text (toString feature.points ++ " points") ]
        , p [] [ text (viewDateTimeString feature.createdAt) ]
        , text feature.title
        , hr [] []
        , text feature.description
        ]


viewDateTimeString : Time.Posix -> String
viewDateTimeString time =
    String.fromInt (Time.toDay Time.utc time)
        ++ " "
        ++ toString (Time.toMonth Time.utc time)
        ++ " "
        ++ String.fromInt (Time.toYear Time.utc time)
        ++ ", "
        ++ toString (Time.toWeekday Time.utc time)
        ++ " "
        ++ String.fromInt (Time.toHour Time.utc time)
        ++ ":"
        ++ String.fromInt (Time.toMinute Time.utc time)


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []
