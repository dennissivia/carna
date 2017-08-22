module Validated exposing (..)


type ValidatedValue a
    = Initial
    | Valid a
    | Invalid String String


isValid : ValidatedValue a -> Bool
isValid value =
    case value of
        Valid _ ->
            True

        _ ->
            False


fromResult : Result String a -> String -> ValidatedValue a
fromResult result rawInput =
    case result of
        Ok value ->
            Valid value

        Err err ->
            Invalid rawInput err


fromMaybe : Maybe a -> ValidatedValue a
fromMaybe maybe =
    case maybe of
        Nothing ->
            Invalid "" ""

        Just x ->
            Valid x


toMaybe : ValidatedValue a -> Maybe a
toMaybe value =
    case value of
        Valid x ->
            Just x

        Initial ->
            Nothing

        _ ->
            Nothing
