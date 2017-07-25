module WelcomeContent exposing (..)

import I18n exposing (Locale(..), Key(..))


type alias CardContent =
    { head : String
    , subhead : Maybe String
    , content : String
    }


bmiInfo : Locale -> CardContent
bmiInfo locale =
    let
        t_ =
            I18n.t locale

        head =
            t_ ContentHeadBMI

        subhead =
            Just <| t_ ContentSubheadBMI

        body =
            t_ ContentBodyBMI
    in
        CardContent head subhead body


carnaInfo : Locale -> CardContent
carnaInfo locale =
    let
        t_ =
            I18n.t locale

        head =
            t_ ContentHeadCarna

        subhead =
            Just (t_ ContentSubHeadCarna)

        body =
            t_ ContentBodyCarna
    in
        CardContent head subhead body


calipometrie : Locale -> CardContent
calipometrie locale =
    let
        t_ =
            I18n.t locale

        head =
            t_ ContentHeadCalipometrie

        subhead =
            Just <| t_ ContentSubHeadCalipometrie

        body =
            t_ ContentBodyCalipometrie
    in
        CardContent head subhead body


caliperMethods : Locale -> CardContent
caliperMethods locale =
    let
        t_ =
            I18n.t locale

        head =
            t_ ContentHeadCaliperMethods

        subhead =
            Just <| t_ ContentSubHeadCaliperMethods

        body =
            t_ ContentBodyCaliperMethods
    in
        CardContent head subhead body
