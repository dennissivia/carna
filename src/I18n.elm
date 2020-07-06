module I18n exposing (Key(..), Locale(..), t)

-- All translations for a given key


type Key
    = Age
    | Gender
    | GenderOptionMale
    | GenderOptionFemale
    | GenderOptionOther
    | Height
    | Weight
    | Waist
    | Hip
    | Chest
    | Subscapular
    | Armpit
    | Biceps
    | Triceps
    | Abdomen
    | IliacCrest
    | Thigh
    | Calf
    | CalculateBodyIndex
    | ValidationIsNotANumber
    | ValidationShouldNotBeEmpty
    | BodyIndexSurfaceArea
    | BodyIndexWaistHipRatio
    | BodyIndexResultColumnIndexName
    | BodyIndexResultColumnIndexValue
    | BodyIndexResultColumnIndexRating
    | BodyFatMethod3Folds
    | BodyFatMethod4Folds
    | BodyFatMethod7Folds
    | BodyFatMethod9Folds
    | InvalidResultContent
    | YourResultHeading
    | ContentHeadBMI
    | ContentSubheadBMI
    | ContentBodyBMI
    | ContentHeadCarna
    | ContentSubHeadCarna
    | ContentBodyCarna
    | ContentHeadCalipometrie
    | ContentSubHeadCalipometrie
    | ContentBodyCalipometrie
    | ContentHeadCaliperMethods
    | ContentSubHeadCaliperMethods
    | ContentBodyCaliperMethods
    | ContentHeadClassificationInfo
    | ContentSubHeadClassificationInfo
    | ContentBodyClassificationInfo
    | ContentHeadNews
    | ContentSubHeadNews
    | ContentBodyNews


type alias TranslationRecord =
    { german : String
    , english : String
    , french : String
    }


type Locale
    = DE
    | EN
    | FR


t : Locale -> Key -> String
t =
    translate


translate : Locale -> Key -> String
translate language key =
    let
        accessor =
            case language of
                DE ->
                    .german

                EN ->
                    .english

                FR ->
                    .french
    in
    knownTranslations key |> accessor


knownTranslations : Key -> TranslationRecord
knownTranslations key =
    case key of
        Age ->
            { german = "Alter (Jahre)"
            , english = "age (years)"
            , french = "âge"
            }

        Weight ->
            { german = "Gewicht (kg)"
            , english = "weight (kg)"
            , french = "translationMissing"
            }

        Height ->
            { german = "Größe (cm)"
            , english = "height (cm)"
            , french = "translationMissing"
            }

        Waist ->
            { german = "Taille (cm)"
            , english = "waist (cm)"
            , french = "translationMissing"
            }

        Hip ->
            { german = "Hüftumfang (cm)"
            , english = "hip size (cm)"
            , french = "translationMissing"
            }

        Gender ->
            { german = "geschlecht"
            , english = "gender"
            , french = "translationMissing"
            }

        GenderOptionMale ->
            { german = "männlich"
            , english = "male"
            , french = "translationMissing"
            }

        GenderOptionFemale ->
            { german = "weiblich"
            , english = "female"
            , french = "translationMissing"
            }

        GenderOptionOther ->
            { german = "Sonstig"
            , english = "Other"
            , french = "translationMissing"
            }

        CalculateBodyIndex ->
            { german = "Body-Index berechnen"
            , english = "calculate body index"
            , french = "translationMissing"
            }

        InvalidResultContent ->
            { german = "Leider konnten wir kein Ergebnis berechnen. Möglicherweise ist die Eingabe nicht vollständig oder fehlerhaft."
            , english = "Sorry, we were not able to calculate your results. Maybe some inputs are missing or incorrect?"
            , french = "translationMissing"
            }

        BodyIndexSurfaceArea ->
            { german = "BSA"
            , english = "BSA"
            , french = "translationMissing"
            }

        BodyIndexWaistHipRatio ->
            { german = "waist-hip ratio"
            , english = "waist-hip ratio"
            , french = "translationMissing"
            }

        BodyIndexResultColumnIndexName ->
            { german = "Body-Index"
            , english = "body-index"
            , french = "translationMissing"
            }

        BodyIndexResultColumnIndexValue ->
            { german = "Ergebnis"
            , english = "your result"
            , french = "translationMissing"
            }

        BodyIndexResultColumnIndexRating ->
            { german = ""
            , english = ""
            , french = ""
            }

        BodyFatMethod3Folds ->
            { german = "3 Falten"
            , english = "3 folds"
            , french = "translationMissing"
            }

        BodyFatMethod4Folds ->
            { german = "4 Falten"
            , english = "4 folds"
            , french = "translationMissing"
            }

        BodyFatMethod7Folds ->
            { german = "7 Falten"
            , english = "7 folds"
            , french = "translationMissing"
            }

        BodyFatMethod9Folds ->
            { german = "9 Falten"
            , english = "9 folds"
            , french = "translationMissing"
            }

        Chest ->
            { german = "Hautfalte Brust (mm)"
            , english = "skinfold chest (mm)"
            , french = "translationMissing"
            }

        Subscapular ->
            { german = "Hautfalte Schulterblatt (mm)"
            , english = "skinfold shoulderblade (mm)"
            , french = "translationMissing"
            }

        Armpit ->
            { german = "Hautfalte Achsel (mm)"
            , english = "skinfold armpit (mm)"
            , french = "translationMissing"
            }

        Biceps ->
            { german = "Hautfalte Bizeps (mm)"
            , english = "skinfold biceps (mm)"
            , french = "translationMissing"
            }

        Triceps ->
            { german = "Hautfalte Triceps (mm)"
            , english = "skinfold triceps (mm)"
            , french = "translationMissing"
            }

        Abdomen ->
            { german = "Hautfalte Abdomen (mm)"
            , english = "skinfold abdomen (mm)"
            , french = "translationMissing"
            }

        IliacCrest ->
            { german = "Hautfalte Hüfte (mm)"
            , english = "skinfold hip (mm)"
            , french = "translationMissing"
            }

        Thigh ->
            { german = "Hautfalte Oberschenkel (mm)"
            , english = "skinfold thigh (mm)"
            , french = "translationMissing"
            }

        Calf ->
            { german = "Hautfalte Wade (mm)"
            , english = "skinfold calf (mm)"
            , french = "translationMissing"
            }

        YourResultHeading ->
            { german = "Ergebnis"
            , english = "Your results"
            , french = "translationMissing"
            }

        ContentHeadBMI ->
            { german = "BMI - Body Mass Index"
            , english = "BMI - Body Mass Index"
            , french = "BMI - Body Mass Index"
            }

        ContentSubheadBMI ->
            { german = "Berechnen und klassifizieren lassen"
            , english = "Calculation and idividual classification"
            , french = "translationMissing"
            }

        ContentBodyBMI ->
            { german = """Der BMI ist eine Maßzahl für die Bewertung des Körpergewichts eines Menschen.
                          Er soll helfen Untergewicht, Idealgewicht und Übergewicht zu erkennen.
                          Der BMI ist allerdings eher ein grober Richtwert, der u.A. nicht die individuelle
                          Zusammensetzung der Körpermasse aus Fett- und Muskelgewebe berücksichtigt.
                         <u>Formel:</u> Gewicht / (Größe(cm)/100)²) """
            , english = """The BMI is caculated from the weight and height of an individual.
                         It estimates if a person is underweight, has normal weight, is overweight, or obese.
                         However, the BMI does not consider individual factors and should be considered a rough estimate.<br /><br />
                         <u>Formula:</u> weight / (height(cm)/100)²) """
            , french = "translationMissing"
            }

        ContentHeadCarna ->
            { german = "Carna - Body Index"
            , english = "Carna - Body Index"
            , french = "Carna - Body Index"
            }

        ContentSubHeadCarna ->
            { german = "Körper-Index Berechnung"
            , english = "Calculate your body indexes"
            , french = "translationMissing"
            }

        ContentBodyCarna ->
            { german = """Mit dem [Body-Index-Rechner](/#body-index) den ist es möglich
                        den [BMI](https://en.wikipedia.org/wiki/Body_mass_index) (nach WHO),
                        den [BAI](https://en.wikipedia.org/wiki/Body_adiposity_index) (Body Adiposity Index),
                        das Idealgewicht nach Broca index, den Ponderal-Index,
                        das Taille-Hüft-Verhältnis und die Körperoberfläche zu berechnen."""
            , english = """With our [body index calculator](/#body-index), you can calculate your
                         [BMI](https://en.wikipedia.org/wiki/Body_mass_index) (according to WHO),
                         the BAI (Body Adiposity Index),
                         your ideal weight (Broca index), your Ponderal-Index, waist-hip-ratio
                         and your body surface area."""
            , french = "translationMissing"
            }

        ContentHeadCalipometrie ->
            { german = "Calipometrie"
            , english = "Body fat percentage"
            , french = "Carna - Body Index"
            }

        ContentSubHeadCalipometrie ->
            { german = "Fettanteil mittels Calipometrie berechnen"
            , english = "Calculate your body fat percentage with a caliper"
            , french = "translationMissing"
            }

        ContentBodyCalipometrie ->
            { german = """Die [Calipometrie](https://de.wikipedia.org/wiki/Calipometrie) (Kalipermetrie) ist eine Methode zur
                        näherungsweisen Bestimmung des Körperfettanteils mittels Messung der Dicke bestimmter Hautfalten.
                        Verschiedene Methoden unterscheiden sich vor allem darin welche Hautfalten gemessen werden."""
            , english = """With skinfold calipers it is possible to estimate the body fat percentage by measuring
                         the thickness of skinfolds. There are different methods which combine different sets of
                         skinfolds and calculation factors for those."""
            , french = "translationMissing"
            }

        ContentHeadCaliperMethods ->
            { german = "Caliper Berechnungsmethoden"
            , english = "Caliper calculation methods"
            , french = "translationMissing"
            }

        ContentSubHeadCaliperMethods ->
            { german = "Die folgenden Methoden stellen wir bereit"
            , english = "We provide the following methods"
            , french = "translationMissing"
            }

        ContentBodyCaliperMethods ->
            { german = """* 3 Falten Methode nach Jackson/Pollack
* 4 Falten Methode nach dem [NHCA](http://www.nachc.org/)
* 7 Falten Methode nach Jackson & Pollock
* 9 Falten Methode nach Parillo"""
            , english = """* 3 skinfold method Jackson/Pollack
* 4 skinfold method [NHCA](http://www.nachc.org/)
* 7 skinfold method Jackson & Pollock
* 9 skinfold method Parillo"""
            , french = "translationMissing"
            }

        ContentHeadClassificationInfo ->
            { german = "Bewertung der Metriken"
            , english = "Classification"
            , french = "translationMissing"
            }

        ContentSubHeadClassificationInfo ->
            { german = "Wie Carna die Ergebnisse der Berechnung einstuft"
            , english = "How carna classifies calculation results"
            , french = "translationMissing"
            }

        ContentBodyClassificationInfo ->
            { german = """Manche Formeln sind sehr generell und beinhalten keine individuellen Faktoren.
                          Damit Bewertung so objektiv und gesund wir möglich ist, beziehen wir stets zusätzliche Faktoren wie
                          Alter, Geschlecht und Größe in die Bewertung der Ergebnisse ein. Somit können wir u.A. realistische
                          Bewerungen für sehr kleine, junge oder alte Menschen geben."""
            , english = """Some of the body index calculations use very generic formulas that do not account for individual factors.
                           One example is the commonly used BMI.
                           To make sure our recommendation is as good as possible, we are taking additional factors into
                           consideration when classifying the result of the calculation.
                           This includes age, gender and height. This is especially important for the results of people of young
                           and old age."""
            , french = "translationMissing"
            }

        ContentHeadNews ->
            { german = "Neuigkeiten"
            , english = "News"
            , french = "translationMissing"
            }

        ContentSubHeadNews ->
            { german = "Aktuelle Verbesserungen"
            , english = "Recent improvements"
            , french = "translationMissing"
            }

        ContentBodyNews ->
            { german = """Um unsere Bewertung noch mehr auf Gesundheit zu fokussieren haben wir unsere BMI Klassifizierung
                angepasst und das Alter mit folgenden Altersklassen in die Bewertung einbezogen:

0-18 Jahre, 18 - 25 Jahre, ..., 55 - 65, mehr als 65 Jahre. """
            , english = """We updated our BMI classification to improve the health awareness over beauty ideals, by adding individual handling for the following age groups:

0 - 18 years, 18 - 25 years, ..., 55 - 65 years, older than 65 years"""
            , french = "translationMissing"
            }

        _ ->
            { german = "translationMissing"
            , english = "translationMissing"
            , french = "translationMissing"
            }
