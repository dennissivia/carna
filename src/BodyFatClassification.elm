module BodyFatClassification exposing (classifyBodyFat)

import Utils exposing (Age, Classification(..), Gender(..))


type alias ThresholdValues =
    ( Float, Float, Float, Float )


type alias Threshold =
    { min_age : Float
    , values : ThresholdValues
    }


classifyBodyFat : Gender -> Maybe Age -> Maybe Float -> Maybe Classification
classifyBodyFat gender maybeAge maybeBodyFat =
    let
        maybeThresholds =
            Maybe.andThen (\age -> thresholds gender age) maybeAge
    in
    Maybe.map3
        (\_ bodyFat values -> classify bodyFat values)
        maybeAge
        maybeBodyFat
        maybeThresholds


classify : Float -> ThresholdValues -> Classification
classify val tuple =
    let
        ( a, b, c, d ) =
            tuple
    in
    if val < a then
        VeryBad (Just "Way too low")

    else if val > a && val < b then
        Good (Just ":)")

    else if val > b && val < c then
        Bad (Just "too low")

    else if val > c && val < d then
        VeryBad (Just "Too high")

    else
        VeryBad (Just "Way too high")


thresholds : Gender -> Age -> Maybe ThresholdValues
thresholds gender age =
    let
        list =
            case gender of
                Female ->
                    [ { start_age = 0, end_age = 25, values = ( 15, 22.1, 25.0, 29.6 ) }
                    , { start_age = 25, end_age = 30, values = ( 15, 22.0, 25.4, 29.8 ) }
                    , { start_age = 30, end_age = 35, values = ( 16, 22.7, 26.4, 30.5 ) }
                    , { start_age = 35, end_age = 40, values = ( 17, 24.0, 27.7, 31.5 ) }
                    , { start_age = 40, end_age = 45, values = ( 19, 25.6, 29.3, 32.8 ) }
                    , { start_age = 45, end_age = 50, values = ( 20, 27.3, 30.9, 34.1 ) }
                    , { start_age = 50, end_age = 60, values = ( 22, 29.7, 33.1, 36.2 ) }
                    , { start_age = 60, end_age = 200, values = ( 23, 30.7, 34.0, 37.3 ) }
                    ]

                _ ->
                    [ { start_age = 0, end_age = 25, values = ( 9, 14.9, 19.0, 23.3 ) }
                    , { start_age = 25, end_age = 30, values = ( 10, 16.5, 20.3, 24.3 ) }
                    , { start_age = 30, end_age = 35, values = ( 12, 18.0, 21.5, 25.2 ) }
                    , { start_age = 35, end_age = 40, values = ( 13, 19.3, 22.6, 26.1 ) }
                    , { start_age = 40, end_age = 45, values = ( 14, 20.5, 23.6, 26.9 ) }
                    , { start_age = 45, end_age = 50, values = ( 15, 21.5, 24.5, 27.6 ) }
                    , { start_age = 50, end_age = 60, values = ( 17, 22.7, 25.6, 28.7 ) }
                    , { start_age = 60, end_age = 200, values = ( 17, 23.3, 26.2, 29.3 ) }
                    ]
    in
    List.filter (\rec -> rec.start_age <= age && rec.end_age > age) list
        |> List.head
        |> Maybe.map .values
