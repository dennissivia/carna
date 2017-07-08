module BodyIndexClassification exposing (..)

{-|


# BMI

THRESHOLDS=[15,18.5,25,30]
CLASSIFICATIONS=[:lower_critical,:lower_warning,:normal,:upper_warning,:upper_critical]


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


classifyBMI : Float -> String
classifyBMI bmi =
    "good"
