module BodyIndexCalculationTest exposing (..)

import BodyIndexCalculation exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, char, float, int, list, string)
import Main exposing (..)
import Regex exposing (contains, regex)
import Test exposing (..)


suite : Test
suite =
    describe "BodyIndexCalculation"
        [ describe "calculateBMI"
            -- Nest as many descriptions as you like.
            [ test "Calculates an average BMI" <|
                \() ->
                    Expect.equal (calculateBMI (Just 78) (Just 178)) 24.62
            , test "0 height is handled" <|
                \() ->
                    Expect.equal True (isInfinite <| calculateBMI (Just 78) (Just 0))
            ]
        , describe "input validation"
            [ describe "validateChainFloat"
                [ fuzz float "parses numbers" <|
                    \randomNum ->
                        toString randomNum
                            |> validateChainFloat
                            |> Expect.equal (Ok randomNum)
                , fuzz (list char) "does not accept empty strings" <|
                    \chars ->
                        case String.trim (String.fromList chars) of
                            "" ->
                                String.fromList chars
                                    |> validateChainFloat
                                    |> Expect.equal (Err "should not be empty")

                            _ ->
                                Expect.pass
                , fuzz (list char) "does not accept non digit chars" <|
                    \chars ->
                        case contains (regex "[^-+.0-9]") (String.trim <| String.fromList chars) of
                            True ->
                                String.fromList chars
                                    |> validateChainFloat
                                    |> Expect.equal (Err "is not a valid number")

                            _ ->
                                Expect.pass
                ]
            ]
        ]
