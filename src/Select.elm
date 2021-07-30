{-
   Module used to simplify creating select components that can rely on the type system
   Copied module from links below and patch it up abit
   https://gurdiga.com/blog/2017/07/09/select-from-union-type-in-elm/
   https://github.com/gurdiga/xo.elm/blob/ca2eca55dc0e771872bac4e406645bf4a1b819da/src/Widgets/Select.elm

   Using it
   ```elm
   fromValuesWithLabels
      [ ( SortByUnsorted, "-- Sort --" )
      , ( SortByVotesMost, "Votes (Most)" )
      , ( SortByVotesLeast, "Votes (Least)" )
      ]
      SortByUnsorted
      SortFeatures
   ```
   where SortBy.. are union variants and SortFeatures is Msg for update function
-}


module Select exposing (fromValuesWithLabels)

import Html exposing (Html, option, select, text)
import Html.Attributes exposing (selected)
import Html.Events exposing (onInput)


fromValuesWithLabels : List ( a, String ) -> a -> (a -> msg) -> Html msg
fromValuesWithLabels valuesWithLabels defaultValue callback =
    let
        optionForTuple ( value, label ) =
            option [ selected (defaultValue == value) ] [ text label ]

        options valuesWithLabels_ _ =
            List.map optionForTuple valuesWithLabels_

        maybeValueFromLabel l =
            List.filter (\( _, label ) -> label == l) valuesWithLabels
                |> List.head

        valueFromLabel label =
            case maybeValueFromLabel label of
                Nothing ->
                    defaultValue

                Just ( value, _ ) ->
                    value
    in
    select
        [ onInput (callback << valueFromLabel) ]
        (options valuesWithLabels defaultValue)
