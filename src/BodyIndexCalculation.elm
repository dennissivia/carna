module BodyIndexCalculation exposing (calculateBAI, calculateBMI, calculateBrocaIndex, calculatePonderalIndex, calculateSkinSurfaceArea, calculateWaistHipRatio)

import Utils exposing (Gender(..), round2)


{-| Calculate the BMI based on two Result Float inputs
-}
calculateBMI : Maybe Float -> Maybe Float -> Float
calculateBMI weight height =
    Maybe.map2 calculateBMI_ weight height
        |> Maybe.withDefault -1


{-| Formula: (weight / (height / 100 ) ²)
-}
calculateBMI_ : Float -> Float -> Float
calculateBMI_ weight height =
    (weight / ((height / 100) ^ 2))
        |> round2


calculateBAI : Maybe Float -> Maybe Float -> Float
calculateBAI hipSize height =
    Maybe.map2 calculateBAI_ hipSize height
        |> Maybe.withDefault -1


{-| <http://onlinelibrary.wiley.com/doi/10.1038/oby.2011.38/full>
-}
calculateBAI_ : Float -> Float -> Float
calculateBAI_ hipSize height =
    ((hipSize / (height / 100) ^ 1.5) - 18)
        |> round2


calculateBrocaIndex : Maybe Gender -> Maybe Float -> Float
calculateBrocaIndex gender height =
    Maybe.map (calculateBrocaIndex_ gender) height
        |> Maybe.withDefault -1


{-| Formula height - 100 x (female or male factor)
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


calculatePonderalIndex : Maybe Float -> Maybe Float -> Float
calculatePonderalIndex weight height =
    Maybe.map2 calculatePonderalIndex_ weight height
        |> Maybe.withDefault -1


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


calculateSkinSurfaceArea : Maybe Float -> Maybe Float -> Float
calculateSkinSurfaceArea weight height =
    Maybe.map2 calculateSkinSurfaceArea_ weight height
        |> Maybe.withDefault -1


{-| Formula (Du Bois): (0.007184 x (height ^ 0.725) x (weight ^ 0.425))
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


calculateWaistHipRatio : Maybe Float -> Maybe Float -> Float
calculateWaistHipRatio waist hipSize =
    Maybe.map2 calculateWaistHipRatio_ waist hipSize
        |> Maybe.withDefault -1


{-| Formula: waist / hipSize
-}
calculateWaistHipRatio_ : Float -> Float -> Float
calculateWaistHipRatio_ waist hipSize =
    waist
        / hipSize
        |> round2
