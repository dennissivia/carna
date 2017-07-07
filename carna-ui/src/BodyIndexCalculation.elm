module BodyIndexCalculation exposing (calculateBMI, calculateBAI, calculateBrocaIndex, calculatePonderalIndex, calculateSkinSurfaceArea, calculateWaistHipRatio)

{-| This module provides pure calculations of a set body indices
-}


type alias UnsafeFloat =
    Result String Float


{-| Round a Float to two too decimal digits
-}
round2 : Float -> Float
round2 inp =
    round (inp * 100)
        |> toFloat
        |> (flip (/) 100)


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


calculateBAI_ : Float -> Float -> Float
calculateBAI_ hipSize height =
    ((hipSize / (height / 100) ^ 1.5))
        - 18
        |> round2


calculateBrocaIndex : UnsafeFloat -> Float
calculateBrocaIndex height =
    Result.map (\h -> h - 100) height
        |> Result.withDefault -1


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



-- 1       ((measurement.hip_size/(measurement.height/100.0)**1.5 ) - 18).round(2)
-- (flip (/) 100)
-- \x -> x / 100
