module BodyIndexCalculationTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, float, string, char)
import Test exposing (..)
import Regex exposing (contains, regex)
import BodyIndexCalculation exposing (..)
import Main exposing (..)


suite : Test
suite =
    describe "BodyIndexCalculation"
        [ describe "calculateBMI"
            -- Nest as many descriptions as you like.
            [ test "Calculates an average BMI" <|
                \() ->
                    Expect.equal (calculateBMI (Ok 78) (Ok 178)) 24.62
            , test "0 height is handled" <|
                \() ->
                    Expect.equal True (isInfinite <| calculateBMI (Ok 78) (Ok 0))
            ]
        , describe "input validation"
            [ describe "validateAge"
                [ fuzz float "parses numbers" <|
                    \randomNum ->
                        (toString randomNum)
                            |> validateAge
                            |> Expect.equal (Ok randomNum)
                , fuzz (list char) "does not accept empty strings" <|
                    \chars ->
                        case (String.trim (String.fromList chars)) of
                            "" ->
                                String.fromList chars
                                    |> validateAge
                                    |> Expect.equal (Err "should not be empty")

                            _ ->
                                Expect.pass
                , fuzz (list char) "does not accept non digit chars" <|
                    \chars ->
                        case (contains (regex "[^-+.0-9]") (String.trim <| String.fromList chars)) of
                            True ->
                                String.fromList chars
                                    |> validateAge
                                    |> Expect.equal (Err "is not a valid number")

                            _ ->
                                Expect.pass
                ]
            ]
        ]
