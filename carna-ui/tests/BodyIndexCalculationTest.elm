module BodyIndexCalculationTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import BodyIndexCalculation exposing (..)


suite : Test
suite =
    describe "BodyIndexCalculationj"
        [ describe "calculateBMI"
            -- Nest as many descriptions as you like.
            [ test "Calculates an average BMI" <|
                \() ->
                    Expect.equal (calculateBMI (Ok 78) (Ok 178)) 24.62
            ]
        ]
