module Utils exposing (Age, Classification(..), Gender(..), UnsafeFloat, round2)


type alias Age =
    Float


type Gender
    = Male
    | Female
    | GenderOther


type Classification
    = Good (Maybe String)
    | Bad (Maybe String)
    | VeryBad (Maybe String)


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
        |> flip (/) 100
