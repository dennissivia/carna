module WelcomeContent exposing (..)

import I18n exposing (Locale(..), Key(..))


type alias CardContent =
    { head : String
    , subhead : Maybe String
    , content : String
    }


bmiInfo : Locale -> CardContent
bmiInfo locale =
    { head = I18n.t locale ContentHeadBMI
    , subhead = Just <| I18n.t locale ContentSubheadBMI
    , content = I18n.t locale ContentBodyBMI
    }


carnaInfo : Locale -> CardContent
carnaInfo locale =
    { head = I18n.t locale ContentHeadCarna
    , subhead = Just (I18n.t locale ContentSubHeadCarna)
    , content = I18n.t locale ContentBodyCarna
    }


calipometrie : Locale -> CardContent
calipometrie locale =
    { head = I18n.t locale ContentHeadCalipometrie
    , subhead = Just <| I18n.t locale ContentSubHeadCalipometrie
    , content = I18n.t locale ContentBodyCalipometrie
    }


caliperMethods : Locale -> CardContent
caliperMethods locale =
    { head = I18n.t locale ContentHeadCaliperMethods
    , subhead = Just <| I18n.t locale ContentSubHeadCaliperMethods
    , content = I18n.t locale ContentBodyCaliperMethods
    }


preferOldPage : Locale -> CardContent
preferOldPage locale =
    { head = I18n.t locale ContentHeadPreferOldPage
    , subhead = Just <| I18n.t locale ContentSubHeadPreferOldPage
    , content = I18n.t locale ContentBodyPreferOldPage
    }
