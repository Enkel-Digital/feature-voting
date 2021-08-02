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
    }


defaultNewFeature : Feature
defaultNewFeature =
    { title = "", description = "", points = 1, createdAt = Time.millisToPosix 0 }


init : Model
init =
    { newFeature = defaultNewFeature
    , -- Scaffolded values to test UI
      features =
        [ { title = "faster loading time", description = "right now the first load takes very long", points = 0, createdAt = Time.millisToPosix 1627985070000 }
        , { title = "search for item using itemID too", description = "right now can only search using item name and not item ID", points = 2, createdAt = Time.millisToPosix 1627975070000 }
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
      -- Use CreateNewFeature to trigger a command, to get runtime to call NewFeature with current time
      -- NewFeature then use current time and newFeature field in model to create a new feature and append into model.features before clearing the new feature input form
    | CreateNewFeature
    | NewFeature Time.Posix


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

        CreateNewFeature ->
            ( model, Task.perform NewFeature Time.now )

        NewFeature time ->
            let
                newFeature_ =
                    model.newFeature

                newFeature =
                    { newFeature_ | createdAt = time }
            in
            ( { model | features = newFeature :: model.features, newFeature = defaultNewFeature }, Cmd.none )



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
        , viewCreateNewFeature model.newFeature
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


viewCreateNewFeature : Feature -> Html Msg
viewCreateNewFeature newFeature =
    div []
        [ viewInput "text" (Just "Title") newFeature.title SetTitle
        , viewInput "text" (Just "Description") newFeature.description SetDescription
        , viewInput "number" Nothing (toString newFeature.points) (SetPoints << Maybe.withDefault 1 << String.toInt)
        , button [ Html.Events.onClick CreateNewFeature ] [ text "New" ]
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


viewInput : String -> Maybe String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder (Maybe.withDefault "" p), value v, onInput toMsg ] []
