module WelcomeContent exposing (..)

import I18n exposing (Key(..), Locale(..))


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


classificationInfo : Locale -> CardContent
classificationInfo locale =
    { head = I18n.t locale ContentHeadClassificationInfo
    , subhead = Just <| I18n.t locale ContentSubHeadClassificationInfo
    , content = I18n.t locale ContentBodyClassificationInfo
    }


news : Locale -> CardContent
news locale =
    { head = I18n.t locale ContentHeadNews
    , subhead = Just <| I18n.t locale ContentSubHeadNews
    , content = I18n.t locale ContentBodyNews
    }
