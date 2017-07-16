module BodyFatCalculation exposing (Skinfolds, caliper3foldsJp, caliper4foldsNhca, caliper7foldsJp, caliper9foldsParillo)

import Utils exposing (Gender(..), Age, UnsafeFloat, round2)
import Result.Extra as ResultExtra


type alias Skinfolds =
    { armpit : Result String Float
    , subscapular : Result String Float -- shoulder blade
    , chest : Result String Float
    , triceps : Result String Float
    , biceps : Result String Float
    , abdomen : Result String Float
    , iliacCrest : Result String Float -- Hip
    , thigh : Result String Float
    , calf : Result String Float
    }


{-| Male:
Skinfolds: chest, abdomen, thigh

Algorithm:
#fat [%] = 495 / d - 450
#d = 1.083800 - 0.0008267*S + 0.0000016*S² - 0.0002574*A
#S = sum of all three folds
#A = age in years

Female:
Skinfolds: triceps, abdomen, hip

#fat [%] = 495 / d - 450
#d = 1.089733 - 0.000924*S + 0.0000056*S² - 0.00012828*A
#S = sum of all three folds
#A = age in years

-}
caliper3foldsJp : Skinfolds -> Gender -> Result String Age -> Maybe Float
caliper3foldsJp skinFolds gender age =
    case gender of
        Female ->
            Result.map3 (\x y z -> x + y + z) skinFolds.triceps skinFolds.abdomen skinFolds.iliacCrest
                |> Result.map2
                    (\age_ sum ->
                        (1.089733 - (0.000924 * sum) + (0.0000056 * (sum ^ 2)) - (0.00012828 * age_))
                    )
                    age
                |> Result.map
                    (\d -> 495 / d - 450)
                |> Result.map round2
                |> Result.toMaybe

        _ ->
            Result.map3 (\x y z -> x + y + z) skinFolds.chest skinFolds.abdomen skinFolds.thigh
                |> Result.map2
                    (\age_ sum ->
                        (1.10938 - (0.0008267 * sum) + (0.0000016 * (sum ^ 2)) - (0.0002574 * age_))
                    )
                    age
                |> Result.map
                    (\d -> 495 / d - 450)
                |> Result.map round2
                |> Result.toMaybe


{-| Caliper4foldsNhca
S = armpit + shoulderblade + chest + abdomen
(0.27784*S)-(0.00053 * S^2) + (0.12437*@data.age-3.28791)
-}
caliper4foldsNhca : Skinfolds -> Result String Age -> Maybe Float
caliper4foldsNhca skinFolds age =
    Result.map4 (\a b c d -> a + b + c + d) skinFolds.armpit skinFolds.subscapular skinFolds.chest skinFolds.abdomen
        |> Result.map2
            (\age_ sum ->
                (0.27784 * sum) - (0.00053 * (sum ^ 2)) + (0.12437 * age_ - 3.28791)
            )
            age
        |> Result.map round2
        |> Result.toMaybe


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
d = 1.112 - 0.00043499*S + 0.00000055*S² - 0.00028826*A
S = Sum of skinfolds
A = Age in years

Female:
d = 1.0970 - 0.00046971*S + 0.00000056*S² - 0.00012828*A
S = Sum of skinfolds
A = Age in years

-}
caliper7foldsJp : Skinfolds -> Gender -> Result String Age -> Maybe Float
caliper7foldsJp skinFolds gender age =
    let
        skinFolds_ =
            ResultExtra.combine
                [ skinFolds.chest
                , skinFolds.subscapular
                , skinFolds.armpit
                , skinFolds.triceps
                , skinFolds.abdomen
                , skinFolds.iliacCrest
                , skinFolds.thigh
                ]

        sum =
            Result.map List.sum skinFolds_
    in
        case gender of
            Female ->
                sum
                    |> Result.map2
                        (\age_ s ->
                            (1.097 - 0.00046971 * s + 0.00000056 * s ^ 2 - 0.00012828 * age_)
                        )
                        age
                    |> Result.map
                        (\d -> 495 / d - 450)
                    |> Result.map round2
                    |> Result.toMaybe

            _ ->
                sum
                    |> Result.map2
                        (\age_ s ->
                            (1.112 - 0.00043499 * s + 0.00000055 * s ^ 2 - 0.00028826 * age_)
                        )
                        age
                    |> Result.map
                        (\d -> 495 / d - 450)
                    |> Result.map round2
                    |> Result.toMaybe


{-| Caliper9foldsParillo:

    def value
      sum=@data.skinfold_chest+@data.skinfold_shoulderblade+@data.skinfold_armpit+@data.skinfold_triceps+@data.skinfold_biceps+
        @data.skinfold_abdomen+@data.skinfold_hip+@data.skinfold_thigh+@data.skinfold_calf
      (27 * sum / (@data.weight / 0.454))
    end

-}
caliper9foldsParillo : Skinfolds -> Result String Float -> Maybe Float
caliper9foldsParillo skinFolds weight =
    let
        skinFolds_ =
            ResultExtra.combine
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
            Result.map List.sum skinFolds_
    in
        Result.map2 (\s weight_ -> 27 * s / (weight_ / 0.454)) sum weight
            |> Result.map round2
            |> Result.toMaybe
