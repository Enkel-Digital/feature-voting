module Main exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as Decode
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
    { id : Int
    , title : String
    , description : String
    , points : Int
    , createdAt : Time.Posix
    }


type Error
    = MissingTitle
    | NotEnoughPoints


type alias User =
    { name : String
    , pointsLeft : Int
    }


type alias Model =
    { error : Maybe Error
    , newFeature : Feature
    , features : List Feature
    , user : User

    -- Points to vote for an existing feature
    , pointsToVote : Int
    }


defaultNewFeature : Feature
defaultNewFeature =
    { id = 0, title = "", description = "", points = 1, createdAt = Time.millisToPosix 0 }


init : Model
init =
    { error = Nothing
    , newFeature = defaultNewFeature

    -- Scaffolded values to test UI
    , features =
        [ { id = 1, title = "faster loading time", description = "right now the first load takes very long", points = 0, createdAt = Time.millisToPosix 1627985070000 }
        , { id = 2, title = "search for item using itemID too", description = "right now can only search using item name and not item ID", points = 2, createdAt = Time.millisToPosix 1627975070000 }
        ]

    -- Scaffolded values to test UI
    , user = { name = "JJ", pointsLeft = 10 }

    -- By default each vote uses 1 point
    , pointsToVote = 1
    }



-- UPDATE


type Msg
    = NoOp
    | ClearError
      -- Sort the features by a given sort method
    | SortFeatures SortMethod
      -- Set values of input into model for new feature
    | SetTitle String
    | SetDescription String
    | SetPoints Int
      -- Use CreateNewFeature to trigger a command, to get runtime to call NewFeature with current time
      -- NewFeature then use current time and newFeature field in model to create a new feature and append into model.features before clearing the new feature input form
    | CreateNewFeature
    | NewFeature Time.Posix
      -- Set how much points, and vote for an existing feature
    | SetPointsToVote Int
    | VoteForFeature Int


type SortMethod
    = SortByUnsorted
    | SortByVotesMost
    | SortByVotesLeast
    | SortByTimeLatest
    | SortByTimeOldest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ClearError ->
            ( { model | error = Nothing }, Cmd.none )

        SetTitle title ->
            let
                newFeature =
                    model.newFeature
            in
            ( { model | newFeature = { newFeature | title = title } }, Cmd.none )

        SetDescription description ->
            let
                newFeature =
                    model.newFeature
            in
            ( { model | newFeature = { newFeature | description = description } }, Cmd.none )

        SetPoints points ->
            let
                newFeature_ =
                    model.newFeature

                newFeature =
                    { newFeature_
                        | -- Force points to always be at least 1
                          -- Force points to be at most user's points left
                          points =
                            if points < 1 then
                                1

                            else if points > model.user.pointsLeft then
                                model.user.pointsLeft

                            else
                                points
                    }
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
                newFeature =
                    model.newFeature
            in
            ( if newFeature.points > model.user.pointsLeft then
                { model | error = Just NotEnoughPoints }

              else if String.isEmpty newFeature.title then
                { model | error = Just MissingTitle }

              else
                -- Add new feature to features list if no errors
                { model
                    | features = { newFeature | createdAt = time } :: model.features
                    , newFeature = defaultNewFeature

                    -- Reset error just in case it was set
                    , error = Nothing
                }
            , Cmd.none
            )

        SetPointsToVote points ->
            ( { model
                | -- Force points to always be at least 1
                  -- Force points to be at most user's points left
                  pointsToVote =
                    if points < 1 then
                        1

                    else if points > model.user.pointsLeft then
                        model.user.pointsLeft

                    else
                        points
              }
            , Cmd.none
            )

        VoteForFeature featureID ->
            let
                featureToVoteFor =
                    case
                        model.features
                            |> List.filter (\feature -> feature.id == featureID)
                            |> List.head
                    of
                        Nothing ->
                            Debug.todo "@todo IMPOSSIBLE state ... how to not even be here???"

                        Just feature ->
                            feature
            in
            ( { model
                | -- Reset back to default of 1
                  pointsToVote = 1

                -- @todo Cannot do like this, need to remove the original one or replace it instead of just adding a new one
                , features = { featureToVoteFor | points = featureToVoteFor.points + model.pointsToVote } :: model.features
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewModal model.error
        , h1 [] [ text "All features" ]

        -- , select
        --     [ onInput (\t -> SortFeatures VotesMost) ]
        --     [ optgroup [ Html.Attributes.attribute "label" "By Votes" ]
        --         [ viewOption VotesMost option [] [ "Votes (Most)" ]
        --         , viewOption VotesLeast option [] [ "Votes (Least)" ]
        --         ]
        --     , optgroup [ Html.Attributes.attribute "label" "By Time" ]
        --         [ viewOption TimeLatest option [] [ "Time (Latest)" ]
        --         , viewOption TimeOldest option [] [ "Time (Oldest)" ]
        --         ]
        --     ]
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
        , viewUser model.user
        ]


viewModal : Maybe Error -> Html Msg
viewModal maybeError =
    case maybeError of
        Just error ->
            -- Error modal is made of container that covers the whole screen in grey overlay, and a inner white box in the middle of screen
            div
                [ Html.Events.onClick ClearError
                , style "position" "absolute"
                , style "top" "0"
                , style "bottom" "0"
                , style "right" "0"
                , style "left" "0"
                , style "display" "flex"
                , style "align-items" "center"
                , style "justify-content" "center"
                , style "background-color" "rgba(40, 40, 40, 0.3)"
                ]
                [ div
                    [ onClickStopPropagation NoOp
                    , style "border-style" "solid"
                    , style "border-radius" "3px"
                    , style "border-color" "white"
                    , style "background-color" "white"
                    , style "height" "50vh"
                    , style "width" "60vw"
                    , style "display" "flex"
                    , style "flex-direction" "column"

                    -- , style "align-items" "center"
                    , style "justify-content" "center"
                    , style "padding" "2em"
                    ]
                    [ span [] [ text ("Error: " ++ toString error) ]
                    , br [] []
                    , button [ Html.Events.onClick ClearError ] [ text "Ok" ]
                    ]
                ]

        Nothing ->
            text ""


onClickStopPropagation : msg -> Html.Attribute msg
onClickStopPropagation msg =
    Html.Events.stopPropagationOn "click" <| Decode.succeed ( msg, True )


viewFeatures : List Feature -> Html Msg
viewFeatures features =
    ul []
        (List.map (\feature -> li [] [ viewFeature feature ]) features)


viewFeature : Feature -> Html Msg
viewFeature feature =
    div []
        [ p [] [ text (toString feature.points ++ " points") ]

        -- @todo Should be in another view, when clicked into a feature to see more then can vote for it. If not all the features will have the same input value going up as you click a single one
        -- , viewInput "number" Nothing (toString model.pointsToVote) (SetPointsToVote << Maybe.withDefault 1 << String.toInt)
        -- , button [ Html.Events.onClick VoteForFeature ] [ text "Vote" ]
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


viewInput : String -> Maybe String -> String -> (String -> Msg) -> Html Msg
viewInput t p v toMsg =
    input [ type_ t, placeholder (Maybe.withDefault "" p), value v, onInput toMsg ] []


viewUser : User -> Html Msg
viewUser user =
    div []
        [ p [] [ text user.name ]
        , p [] [ text ("Points Left: " ++ String.fromInt user.pointsLeft) ]
        ]
