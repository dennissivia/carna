module BodyIndexCalculation exposing (calculateBMI, calculateBAI, calculateBrocaIndex, calculatePonderalIndex, calculateSkinSurfaceArea, calculateWaistHipRatio)

import Utils exposing (Gender(..), UnsafeFloat, round2)


{-| Calculate the BMI based on two Result Float inputs
-}
calculateBMI : UnsafeFloat -> UnsafeFloat -> Float
calculateBMI weight height =
    Result.map2 calculateBMI_ weight height
        |> Result.withDefault -1


{-| Formula: (weight / (height / 100 ) ^ 2)
-}
calculateBMI_ : Float -> Float -> Float
calculateBMI_ weight height =
    (weight / ((height / 100) ^ 2))
        |> round2


calculateBAI : UnsafeFloat -> UnsafeFloat -> Float
calculateBAI hipSize height =
    Result.map2 calculateBAI_ hipSize height
        |> Result.withDefault -1


{-| <http://onlinelibrary.wiley.com/doi/10.1038/oby.2011.38/full>
-}
calculateBAI_ : Float -> Float -> Float
calculateBAI_ hipSize height =
    ((hipSize / (height / 100) ^ 1.5) - 18)
        |> round2


calculateBrocaIndex : Maybe Gender -> UnsafeFloat -> Float
calculateBrocaIndex gender height =
    Result.map (calculateBrocaIndex_ gender) height
        |> Result.withDefault -1


{-| Formula height - 100 * (female or male factor)
-}
calculateBrocaIndex_ : Maybe Gender -> Float -> Float
calculateBrocaIndex_ gender height =
    let
        baseWeight =
            height - 100
    in
        case Maybe.withDefault GenderOther gender of
            Female ->
                (baseWeight * 0.8)
                    |> round2

            _ ->
                (baseWeight * 0.9)
                    |> round2


calculatePonderalIndex : UnsafeFloat -> UnsafeFloat -> Float
calculatePonderalIndex weight height =
    Result.map2 calculatePonderalIndex_ weight height
        |> Result.withDefault -1


{-| Formula: weight / ((height / 100.0) ^ 3)
-}
calculatePonderalIndex_ : Float -> Float -> Float
calculatePonderalIndex_ weight height =
    let
        denominator =
            (height / 100) ^ 3
    in
        weight
            / denominator
            |> round2


calculateSkinSurfaceArea : UnsafeFloat -> UnsafeFloat -> Float
calculateSkinSurfaceArea weight height =
    Result.map2 calculateSkinSurfaceArea_ weight height
        |> Result.withDefault -1


{-| Formula (Du Bois): (0.007184* (height ^ 0.725) * (weight ^ 0.425))
-}
calculateSkinSurfaceArea_ : Float -> Float -> Float
calculateSkinSurfaceArea_ weight height =
    let
        a =
            0.007184

        b =
            0.725

        c =
            0.425
    in
        (a * (height ^ b) * (weight ^ c))
            |> round2


calculateWaistHipRatio : UnsafeFloat -> UnsafeFloat -> Float
calculateWaistHipRatio waist hipSize =
    Result.map2 calculateWaistHipRatio_ waist hipSize
        |> Result.withDefault -1


{-| Formula: waist / hipSize
-}
calculateWaistHipRatio_ : Float -> Float -> Float
calculateWaistHipRatio_ waist hipSize =
    waist
        / hipSize
        |> round2
