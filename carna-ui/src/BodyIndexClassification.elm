module BodyIndexClassification exposing (Classification(..), classifyBMI, classifyBrocaIndex)

import List.Extra as ListExtra
import Maybe
import Utils exposing (Gender(..))


{-|


# BAI

Status » BAI Male BAI Female·
-- my -- < 5% < 15%
Underweight < 8% » < 21%
Healthy 8 to 19% 21 to 33%
Overweight 19 to 25% 33 to 39%
Obese · > 25% » > 39%

THRESHOLDS:
:male => [ 5, 8,19,25],
:female => [15,21,33,39]

SCOPE:
measurement.age.nil? || measurement.age < 20 || measurement.age > 40


# Broca Index


## note about only fitting average hight values?

5 weight=self.broca_index(measurement) // height - 100
6 if measurement.female?
7 BodyIndexCalculator.trim_result(weight*0.8+age_offset(measurement.age))
8 else
9 BodyIndexCalculator.trim_result(weight*0.9+age_offset(measurement.age))
10 end


# Ponderal Index

[11,14]


# Surface Area

average values:
general: 1,7 m².
men: 1,9 m².
women: 1,6 m²
12year: 1,33 m²

      def average_value
        if @measurement.age<=12
          1.33
        else
          if @measurement.female?
            1.6
          else
            1.9
          end
        end
      end

      def lower_border(value)
        value*0.8
      end

      def upper_border(value)
        value*1.2
      end


# Waist Hip Ratio

                  female     male

#self defined#
viel zu klein < 0,4 < 0,45 #

Zu Klein < 0,5 < 0,6
Normalgewicht < 0,8» < 0,9
Übergewicht 0,8-0,84 0,9-0,99
Adipositas > 0,85»· > 1,0

CLASSIFICATIONS=[:lower_critical,:lower_warning,:normal,:upper_warning,:upper_critical]

-}
type Classification
    = Good (Maybe String)
    | Bad (Maybe String)
    | VeryBad (Maybe String)


type alias BodyIndexRange =
    { start : Float
    , end : Float
    , class : Classification

    -- , text : String
    }


{-| BMI
starkes Untergewicht < 16,00 Untergewicht
mäßiges Untergewicht 16,0 – < 17
leichtes Untergewicht 17,0 – < 18,5
Normalgewicht 18,5 – < 25 Normalgewicht
Präadipositas 25,0 – < 30 Übergewicht
Adipositas Grad I 30,0 – < 35 Adipositas
Adipositas Grad II 35,0 – < 40
Adipositas Grad III ≥ 40,0
THRESHOLDS=[15,18.5,25,30]
CLASSIFICATIONS=[:lower_critical,:lower_warning,:normal,:upper_warning,:upper_critical]
-}
bmiRanges : List BodyIndexRange
bmiRanges =
    [ { start = 0, end = 16, class = VeryBad Nothing }
    , { start = 16, end = 17, class = Bad Nothing }
    , { start = 17, end = 18.5, class = Bad Nothing }
    , { start = 18.5, end = 25, class = Good Nothing }
    , { start = 25, end = 30, class = Bad Nothing }
    , { start = 30, end = 35, class = VeryBad Nothing }
    , { start = 35, end = 40, class = VeryBad Nothing }
    , { start = 40, end = 1000, class = VeryBad Nothing }
    ]


classifyBMI : Float -> Maybe Classification
classifyBMI bmi =
    let
        match =
            ListExtra.find (\r -> bmi >= r.start && bmi < r.end) bmiRanges
    in
        case match of
            Just rec ->
                Just rec.class

            Nothing ->
                Nothing


{-|


## note about only fitting average hight values?

weight=self.broca_index(measurement) // height - 100
if measurement.female?
BodyIndexCalculator.trim_result(weight*0.8+age_offset(measurement.age))
else
BodyIndexCalculator.trim_result(weight*0.9+age_offset(measurement.age))
end

-}
classifyBrocaIndex : Float -> Classification
classifyBrocaIndex bi =
    let
        -- range =
        --     [ { start = 0, end = bi * 0.95, class = Bad Nothing }
        --     , { start = bi * 0.95, end = bi * 1.05, class = Neutral Nothing }
        --     , { start = bi * 1.05, end = 1000, class = Good Nothing }
        --     ]
        ( lower, upper ) =
            ( bi * 0.95, bi * 1.05 )
    in
        if bi < lower then
            Bad (Just "Too low")
        else if bi >= lower && bi < upper then
            Good (Just "Ideal")
        else
            Bad (Just "Too high")
