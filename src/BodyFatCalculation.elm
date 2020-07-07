module BodyFatCalculation exposing (Skinfolds, caliper3foldsJp, caliper4foldsNhca, caliper7foldsJp, caliper9foldsParillo)

import Maybe.Extra as MaybeExtra
import Utils exposing (Age, Gender(..), UnsafeFloat, round2)


{-| used for calculation
-}
type alias Skinfolds =
    { armpit : Maybe Float
    , subscapular : Maybe Float -- shoulder blade
    , chest : Maybe Float
    , triceps : Maybe Float
    , biceps : Maybe Float
    , abdomen : Maybe Float
    , iliacCrest : Maybe Float -- Hip
    , thigh : Maybe Float
    , calf : Maybe Float
    }


{-| Male:
Skinfolds: chest, abdomen, thigh

Algorithm:
#fat [%] = 495 / d - 450
#d = 1.083800 - 0.0008267 x S + 0.0000016 x S² - 0.0002574 x A
#S = sum of all three folds
#A = age in years

Female:
Skinfolds: triceps, abdomen, hip

#fat [%] = 495 / d - 450
#d = 1.089733 - 0.000924 x S + 0.0000056 x S² - 0.00012828 x A
#S = sum of all three folds
#A = age in years

-}
caliper3foldsJp : Skinfolds -> Gender -> Maybe Age -> Maybe Float
caliper3foldsJp skinFolds gender age =
    case gender of
        Female ->
            Maybe.map3 (\x y z -> x + y + z) skinFolds.triceps skinFolds.abdomen skinFolds.iliacCrest
                |> Maybe.map2
                    (\age_ sum ->
                        1.089733 - (0.000924 * sum) + (0.0000056 * (sum ^ 2)) - (0.00012828 * age_)
                    )
                    age
                |> Maybe.map
                    (\d -> 495 / d - 450)
                |> Maybe.map round2

        _ ->
            Maybe.map3 (\x y z -> x + y + z) skinFolds.chest skinFolds.abdomen skinFolds.thigh
                |> Maybe.map2
                    (\age_ sum ->
                        1.10938 - (0.0008267 * sum) + (0.0000016 * (sum ^ 2)) - (0.0002574 * age_)
                    )
                    age
                |> Maybe.map
                    (\d -> 495 / d - 450)
                |> Maybe.map round2


{-| Caliper4foldsNhca
S = armpit + shoulderblade + chest + abdomen
(0.27784 x S)-(0.00053 x S²) + (0.12437 x @data.age-3.28791)
-- NOTE git: more narrow types by using maybe instead of result
-}
caliper4foldsNhca : Skinfolds -> Maybe Age -> Maybe Float
caliper4foldsNhca skinFolds age =
    Maybe.map4 (\a b c d -> a + b + c + d) skinFolds.armpit skinFolds.subscapular skinFolds.chest skinFolds.abdomen
        |> Maybe.map2
            (\age_ sum ->
                (0.27784 * sum) - (0.00053 * (sum ^ 2)) + (0.12437 * age_ - 3.28791)
            )
            age
        |> Maybe.map round2


{-| Caliper7foldsJp
Skinfolds:

chest
shoulderblade
armpit
triceps
abdomen
hip
thigh

Algorithm:
%fat = (495.0/d-450.0)

Male:
d = 1.112 - 0.00043499 x S + 0.00000055 x S² - 0.00028826 x A
S = Sum of skinfolds
A = Age in years

Female:
d = 1.0970 - 0.00046971 x S + 0.00000056 x S² - 0.00012828 x A
S = Sum of skinfolds
A = Age in years

-}
caliper7foldsJp : Skinfolds -> Gender -> Maybe Age -> Maybe Float
caliper7foldsJp skinFolds gender age =
    let
        skinFolds_ =
            MaybeExtra.combine
                [ skinFolds.chest
                , skinFolds.subscapular
                , skinFolds.armpit
                , skinFolds.triceps
                , skinFolds.abdomen
                , skinFolds.iliacCrest
                , skinFolds.thigh
                ]

        sum =
            Maybe.map List.sum skinFolds_
    in
    case gender of
        Female ->
            sum
                |> Maybe.map2
                    (\age_ s ->
                        1.097 - 0.00046971 * s + 0.00000056 * s ^ 2 - 0.00012828 * age_
                    )
                    age
                |> Maybe.map
                    (\d -> 495 / d - 450)
                |> Maybe.map round2

        _ ->
            sum
                |> Maybe.map2
                    (\age_ s ->
                        1.112 - 0.00043499 * s + 0.00000055 * s ^ 2 - 0.00028826 * age_
                    )
                    age
                |> Maybe.map
                    (\d -> 495 / d - 450)
                |> Maybe.map round2


{-| Caliper9foldsParillo:

    def value
      sum=skinfold_chest+skinfold_shoulderblade+skinfold_armpit+skinfold_triceps+
      skinfold_biceps+ skinfold_abdomen+skinfold_hip+skinfold_thigh+skinfold_calf

      (27 * sum / (@data.weight / 0.454))
    end

-}
caliper9foldsParillo : Skinfolds -> Maybe Float -> Maybe Float
caliper9foldsParillo skinFolds weight =
    let
        skinFolds_ =
            MaybeExtra.combine
                [ skinFolds.chest
                , skinFolds.subscapular
                , skinFolds.armpit
                , skinFolds.triceps
                , skinFolds.biceps
                , skinFolds.abdomen
                , skinFolds.iliacCrest
                , skinFolds.thigh
                , skinFolds.calf
                ]

        sum =
            Maybe.map List.sum skinFolds_
    in
    Maybe.map2 (\s weight_ -> 27 * s / (weight_ / 0.454)) sum weight
        |> Maybe.map round2
