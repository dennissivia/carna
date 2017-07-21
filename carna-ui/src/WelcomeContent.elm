module WelcomeContent exposing (..)


type alias CardContent =
    ( String, Maybe String, String )


bmiInfo : CardContent
bmiInfo =
    let
        head =
            "About BMI"

        subhead =
            Nothing

        body =
            """
            BMI is awesome, so lets be happy
            """
    in
        ( head, subhead, body )


carnaInfo : CardContent
carnaInfo =
    let
        head =
            "About Carna"

        subhead =
            Just "Carna ermöglicht diverse Körper-Index Berechnungen"

        body =
            """
        Mit dem Body-Index-Rechner den ist es möglich den Körperfettanteil,
        den BMI (nach WHO),  den BAI (Body Adiposity Index),
        das Idealgewicht nach Broca index, den Ponderal-Index,
        das Taille-Hüft-Verhältnis und die Körperoberfläche (nach DuBois) zu berechnen.
"""
    in
        ( head, subhead, body )
