module Main exposing (..)

import String
import Regex
import String.Extra as StringExtra
import Html exposing (programWithFlags, div, text, span, h1, i, Html)
import Html.Attributes exposing (href, class, style, width)
import Material
import Material.Table as Table
import Material.Icon as Icon
import Material.Elevation as Elevation
import Material.Card as Card
import Material.Color as MColor
import Color
import Material.Toggles as Toggles
import Material.Options exposing (Style, css, cs)
import Material.Grid as Grid
import Material.Textfield as Textfield
import Material.Layout as Layout
import Material.Scheme
import Material.Options as Options
import Material.Button as Button
import Material.Options exposing (css)
import Svg exposing (Svg)
import Material.Icons.Social exposing (sentiment_dissatisfied, sentiment_neutral, sentiment_satisfied, sentiment_very_dissatisfied, sentiment_very_satisfied)
import Material.Icons.Alert exposing (error_outline)
import BodyIndexCalculation exposing (..)
import BodyIndexClassification exposing (Classification(..), classifyBMI, classifyBrocaIndex)
import Utils exposing (Gender(..))


type alias Flags =
    { userLanguage : String }


type alias Model =
    { count : Int
    , mdl : Material.Model
    , selectedTab : Int
    , userLanguage : String
    , bodyIndex : BodyIndex
    , bodyIndexSubmitted : Bool
    }


type alias BodyIndex =
    { age : Result String Int
    , height : Result String Float
    , weight : Result String Float
    , waist : Result String Float
    , hipSize : Result String Float
    , gender : Maybe Gender
    , result : Maybe BodyIndexResult
    , isValid : Bool
    }


type alias BodyIndexResult =
    { bmi : Float
    , bai : Float
    , brocaIndex : Float
    , ponderalIndex : Float
    , surfaceArea : Float
    , whRatio : Float
    }


type alias BodyIndexResultRating =
    { bmi : BodyIndexSatisfaction
    , bai : BodyIndexSatisfaction
    , brocaIndex : BodyIndexSatisfaction
    , ponderalIndex : BodyIndexSatisfaction
    , surfaceArea : BodyIndexSatisfaction
    , whRatio : BodyIndexSatisfaction
    }


type Msg
    = SelectTab Int
    | BodyIndexSubmit
    | SelectGender Gender
    | Mdl (Material.Msg Msg)
    | BodyIndexChange BodyIndexMsg


type BodyIndexMsg
    = SetAge String
    | SetHeight String
    | SetWeight String
    | SetWaist String
    | SetHip String
    | SetGender Gender


type BodyIndexSatisfaction
    = VerySatisfied
    | Satisfied
    | Neutral
    | Dissatisfied
    | VeryDissatisfied
    | SatisfactionUnknown


type alias Mdl =
    Material.Model


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags, Cmd.none )


initialBodyIndex : BodyIndex
initialBodyIndex =
    { age = Ok 27
    , height = Ok 165.5
    , weight = Ok 75
    , waist = Ok 85.2
    , hipSize = Ok 100.3
    , gender = Nothing
    , result = Nothing
    , isValid = True
    }


initialModel : Flags -> Model
initialModel flags =
    let
        mdl =
            Material.model
    in
        { count = 0
        , mdl = Layout.setTabsWidth 200 mdl
        , selectedTab = 0
        , userLanguage = flags.userLanguage
        , bodyIndex = initialBodyIndex
        , bodyIndexSubmitted = False
        }


primaryColor : MColor.Hue
primaryColor =
    MColor.Green


accentColor : MColor.Hue
accentColor =
    MColor.Yellow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BodyIndexSubmit ->
            let
                newBodyIndex =
                    calculateBodyIndex model.bodyIndex
            in
                { model | bodyIndexSubmitted = True, bodyIndex = newBodyIndex } ! []

        BodyIndexChange bodyIndexMessage ->
            let
                newBodyIndex =
                    updateBodyIndex model.bodyIndex bodyIndexMessage
            in
                { model | bodyIndex = newBodyIndex } ! []

        SelectGender gender ->
            let
                newBodyIndex =
                    updateBodyIndex model.bodyIndex <| SetGender gender
            in
                { model | bodyIndex = newBodyIndex } ! []

        SelectTab num ->
            let
                newModel =
                    { model | selectedTab = num }
            in
                newModel ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model


updateBodyIndex : BodyIndex -> BodyIndexMsg -> BodyIndex
updateBodyIndex bodyIndex msg =
    let
        newBodyIndex =
            case msg of
                SetAge newAge ->
                    { bodyIndex | age = (validateAge newAge) }

                SetHeight newHeight ->
                    { bodyIndex | height = (validateHeight newHeight) }

                SetWeight newWeight ->
                    { bodyIndex | weight = (validateWeight newWeight) }

                SetWaist newWaist ->
                    { bodyIndex | waist = (validateWaist newWaist) }

                SetHip newHip ->
                    { bodyIndex | hipSize = (validateHip newHip) }

                SetGender gender ->
                    { bodyIndex | gender = Just gender }
    in
        { newBodyIndex | isValid = validateBodyIndex newBodyIndex }


calculateBodyIndex : BodyIndex -> BodyIndex
calculateBodyIndex bodyIndex =
    case bodyIndex.isValid of
        True ->
            let
                newBodyIndexResult =
                    Just
                        { bmi = calculateBMI bodyIndex.weight bodyIndex.height
                        , bai = calculateBAI bodyIndex.hipSize bodyIndex.height
                        , brocaIndex = calculateBrocaIndex bodyIndex.gender bodyIndex.height
                        , ponderalIndex = calculatePonderalIndex bodyIndex.weight bodyIndex.height
                        , surfaceArea = calculateSkinSurfaceArea bodyIndex.weight bodyIndex.height
                        , whRatio = calculateWaistHipRatio bodyIndex.waist bodyIndex.hipSize
                        }
            in
                { bodyIndex | result = newBodyIndexResult }

        False ->
            { bodyIndex | result = Nothing }


{-| TODO: use Classification module instead
-}
classifyBodyIndex : BodyIndexResult -> BodyIndexResultRating
classifyBodyIndex bodyIndexResult =
    { bmi = classificationToSatisfaction <| classifyBMI bodyIndexResult.bmi
    , bai = Satisfied
    , brocaIndex = classificationToSatisfaction <| Just (classifyBrocaIndex bodyIndexResult.brocaIndex)
    , ponderalIndex = SatisfactionUnknown
    , surfaceArea = SatisfactionUnknown
    , whRatio = SatisfactionUnknown
    }


classificationToSatisfaction : Maybe Classification -> BodyIndexSatisfaction
classificationToSatisfaction class =
    case class of
        Nothing ->
            SatisfactionUnknown

        Just class ->
            case class of
                Good text ->
                    Satisfied

                Bad text ->
                    Neutral

                VeryBad text ->
                    Dissatisfied


validateBodyIndex : BodyIndex -> Bool
validateBodyIndex bodyIndex =
    Result.map5 (\_ _ _ _ _ -> True)
        bodyIndex.age
        bodyIndex.height
        bodyIndex.weight
        bodyIndex.waist
        bodyIndex.hipSize
        |> Result.withDefault False


validateAge : String -> Result String Int
validateAge =
    validateChainInt "Age"


validateHeight : String -> Result String Float
validateHeight =
    validateChainFloat "Height"


validateWeight : String -> Result String Float
validateWeight =
    validateChainFloat "Weight"


validateWaist : String -> Result String Float
validateWaist =
    validateChainFloat "Waist"


validateHip : String -> Result String Float
validateHip =
    validateChainFloat "Hip"


validateChainFloat : String -> String -> Result String Float
validateChainFloat label val =
    Ok val
        |> Result.andThen (validatePresence label)
        |> Result.andThen (validateFloat label)


validateChainInt : String -> String -> Result String Int
validateChainInt label val =
    Ok val
        |> Result.andThen (validatePresence label)
        |> Result.andThen (validateInt label)


validateFloat : String -> String -> Result String Float
validateFloat label str =
    case Regex.contains (Regex.regex "^[+-]?[0-9]+([.][0-9]+)?$") str of
        True ->
            String.toFloat str

        False ->
            Err <| label ++ " is not a valid number"


validateInt : String -> String -> Result String Int
validateInt label str =
    case Regex.contains (Regex.regex "^[+-]?[0-9]+$") str of
        True ->
            String.toInt str

        False ->
            Err <| label ++ " is not a valid number"


validatePresence : String -> String -> Result String String
validatePresence label str =
    if not (StringExtra.isBlank str) then
        Ok str
    else
        Err <| label ++ " should not be empty"


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme primaryColor accentColor <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.selectedTab model.selectedTab
            , Layout.onSelectTab SelectTab
            ]
            { header =
                [ Layout.row
                    [ Options.nop, css "transition" "height 333ms ease-in-out 0s" ]
                    [ Layout.title [] [ text "Carna" ]
                    , Layout.spacer
                    , Layout.navigation []
                        [ Layout.link
                            []
                            [ Icon.i "photo" ]
                        , Layout.link
                            [ Layout.href "https://github.com/debois/elm-mdl" ]
                            [ span [] [ text "github" ] ]
                        , Layout.link
                            [ Layout.href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
                            [ text "elm-package" ]
                        ]
                    ]
                ]
            , drawer =
                [ Layout.title [] [ text "Carna" ]
                , Layout.navigation
                    []
                    [ Layout.link
                        [ Layout.href "https://github.com/debois/elm-mdl" ]
                        [ text "github" ]
                    , Layout.link
                        [ Layout.href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
                        [ text "elm-package" ]
                    , Layout.link
                        [ Layout.href "#cards"
                        , Options.onClick (Layout.toggleDrawer Mdl)
                        ]
                        [ text "Card component" ]
                    ]
                ]
            , tabs =
                ( [ text "Body Index", text "Body Fat Calc", text <| "Browser language: " ++ model.userLanguage ]
                , [ MColor.background (MColor.color primaryColor MColor.S400)
                  ]
                )
            , main = [ viewBody model ]
            }


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        0 ->
            div [] [ viewBodyIndexForm model ]

        1 ->
            div [] []

        -- viewBodyFatForm model ]
        _ ->
            viewBodyIndexForm model


gridCell : List (Style a) -> List (Html a) -> Grid.Cell a
gridCell styling =
    Grid.cell <| List.concat [ styling ]


viewBodyIndexForm : Model -> Html Msg
viewBodyIndexForm model =
    let
        gridStyle =
            [ Grid.size Grid.All 12, Grid.size Grid.Desktop 6, css "padding" "3rem" ]
    in
        [ gridCell gridStyle
            [ div
                []
                [ viewBodyIndexGenderSelect model
                , textField model.mdl 0 "Age" (model.bodyIndex.age) (BodyIndexChange << SetAge)
                , textField model.mdl 1 "Height" (model.bodyIndex.height) (BodyIndexChange << SetHeight)
                , textField model.mdl 2 "Weight" (model.bodyIndex.weight) (BodyIndexChange << SetWeight)
                , textField model.mdl 3 "Waist" (model.bodyIndex.waist) (BodyIndexChange << SetWaist)
                , textField model.mdl 4 "Hip" (model.bodyIndex.hipSize) (BodyIndexChange << SetHip)
                , Button.render Mdl
                    [ 5 ]
                    model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.onClick BodyIndexSubmit
                    ]
                    [ text "Raised button" ]
                ]
            ]
        , gridCell gridStyle
            [ div []
                [ if model.bodyIndexSubmitted then
                    div [] [ viewBodyIndexResultCard model.bodyIndex ]
                  else
                    div [] []
                ]
            ]
        ]
            |> Grid.grid []


viewBodyIndexGenderSelect : Model -> Html Msg
viewBodyIndexGenderSelect model =
    div
        [ class "gender-select" ]
        [ Toggles.radio Mdl
            [ 0 ]
            model.mdl
            [ Toggles.value <| hasGender model.bodyIndex Female
            , Toggles.group "PersonGenderRadio"
            , Toggles.ripple
            , Options.onToggle <| SelectGender Female
            ]
            [ text "Female" ]
        , Toggles.radio Mdl
            [ 1 ]
            model.mdl
            [ Toggles.value <| hasGender model.bodyIndex Male
            , Toggles.group "PersonGenderRadio"
            , Toggles.ripple
            , Options.onToggle <| SelectGender Male
            ]
            [ text "Male" ]
        ]


viewBodyIndexResultCard : BodyIndex -> Html Msg
viewBodyIndexResultCard bodyIndex =
    Card.view
        [ css "height" "100%"
        , css "width" "320px"
        , MColor.background (MColor.color MColor.Brown MColor.S500)
        , Elevation.e8

        -- , if model.raised == k then
        -- Elevation.e8
        -- else
        -- Elevation.e2
        -- , Elevation.transition 250
        -- , Options.onMouseEnter (Raise k)
        -- , Options.onMouseLeave (Raise -1)
        ]
        [ Card.title
            []
            [ Card.head
                [ MColor.text MColor.white ]
                [ if bodyIndex.isValid then
                    viewBodyIndexResulTable bodyIndex
                  else
                    div [] [ text "invalid input" ]
                ]
            ]
        ]



-- faces:
-- https://material.io/icons/#ic_sentiment_dissatisfied
-- sentiment_satisfied, sentiment_dissatisfied, sentiment_neutral,
-- sentiment_very_satisfied, sentiment_very_dissatisfied


viewBodyIndexResulTable : BodyIndex -> Html Msg
viewBodyIndexResulTable bodyIndex =
    case bodyIndex.result of
        Nothing ->
            div [] []

        Just result ->
            let
                bodyIndexRating =
                    classifyBodyIndex result
            in
                Table.table [ css "width" "100%", cs "body-index-result-table" ]
                    [ Table.thead []
                        [ Table.tr []
                            [ Table.th [] [ text "Body index" ]
                            , Table.th [] [ text "Value" ]
                            , Table.th [] [ text "Rating" ]
                            ]
                        ]
                    , Table.tbody []
                        [ viewBodyIndexResultRow "BMI WHO" (toString result.bmi) bodyIndexRating.bmi
                        , viewBodyIndexResultRow "BAI" (toString result.bai) bodyIndexRating.bai
                        , viewBodyIndexResultRow "Broca Index" (toString result.brocaIndex) bodyIndexRating.brocaIndex
                        , viewBodyIndexResultRow "Ponderal Index" (toString result.ponderalIndex) bodyIndexRating.ponderalIndex
                        , viewBodyIndexResultRow "Skin Surface Area" (toString result.surfaceArea) bodyIndexRating.surfaceArea
                        , viewBodyIndexResultRow "Waist-Hip ratio" (toString result.whRatio) bodyIndexRating.whRatio
                        ]
                    ]


viewBodyIndexResultRow : String -> String -> BodyIndexSatisfaction -> Html Msg
viewBodyIndexResultRow name value satisfaction =
    Table.tr
        []
        [ Table.td [] [ text name ]
        , Table.td [ Table.numeric ] [ text value ]
        , Table.td [ Table.numeric ] [ satisfactionIcon satisfaction ]
        ]


satisfactionIcon : BodyIndexSatisfaction -> Html Msg
satisfactionIcon satisfaction =
    let
        svgStyle =
            [ width 48 ]
    in
        case satisfaction of
            VerySatisfied ->
                Svg.svg svgStyle [ sentiment_very_satisfied Color.green 24 ]

            Satisfied ->
                Svg.svg svgStyle [ sentiment_satisfied Color.green 24 ]

            Neutral ->
                Svg.svg svgStyle [ sentiment_neutral Color.black 24 ]

            Dissatisfied ->
                Svg.svg svgStyle [ sentiment_dissatisfied Color.red 24 ]

            VeryDissatisfied ->
                Svg.svg svgStyle [ sentiment_very_dissatisfied Color.red 24 ]

            SatisfactionUnknown ->
                Svg.svg svgStyle [ error_outline Color.blue 24 ]


viewBodyFatForm : Model -> Html Msg
viewBodyFatForm model =
    let
        gridStyle =
            [ Grid.size Grid.All 4 ]
    in
        div [ style [ ( "padding", "3rem" ) ] ]
            [ viewBodyFatPersonForm model
            , viewBodyFatSkinFoldForm model
            ]


viewBodyFatGenderForm : Model -> Html Msg
viewBodyFatGenderForm model =
    div
        []
        [ Toggles.radio Mdl
            [ 0 ]
            model.mdl
            [ Toggles.value <| hasGender model.bodyIndex Female
            , Toggles.group "PersonGenderRadio"
            , Toggles.ripple
            , Options.onToggle <| SelectGender Female
            ]
            [ text "Female" ]
        , Toggles.radio Mdl
            [ 1 ]
            model.mdl
            [ Toggles.value <| hasGender model.bodyIndex Male
            , Toggles.group "PersonGenderRadio"
            , Toggles.ripple
            , Options.onToggle <| SelectGender Male
            ]
            [ text "Male" ]
        ]


hasGender : BodyIndex -> Gender -> Bool
hasGender bodyIndex otherGender =
    Maybe.map (\g -> g == otherGender) bodyIndex.gender
        |> Maybe.withDefault False


viewBodyFatPersonForm : Model -> Html Msg
viewBodyFatPersonForm model =
    let
        gridStyle =
            [ Grid.size Grid.All 4 ]
    in
        [ (gridCell) gridStyle
            [ viewBodyFatGenderForm model

            -- , textField model.mdl 0 "Age" ""
            -- , textField model.mdl 1 "Height" ""
            -- , textField model.mdl 2 "Weight" ""
            ]
        ]
            |> Grid.grid []


viewBodyFatSkinFoldForm : Model -> Html Msg
viewBodyFatSkinFoldForm model =
    let
        style =
            [ Grid.size Grid.All 4 ]
    in
        [ (gridCell) style
            [-- textField model.mdl 5 "Chest" ""
             -- , textField model.mdl 6 "Shoulder" ""
             -- , textField model.mdl 7 "Armpit" ""
            ]
        , (gridCell) style
            [--textField model.mdl 8 "Biceps" ""
             -- , textField model.mdl 9 "Triceps" ""
             -- , textField model.mdl 10 "Abdomen" ""
            ]
        , (gridCell) style
            [-- textField model.mdl 11 "Hip" ""
             -- , textField model.mdl 12 "Quad/Thigh" ""
             -- , textField model.mdl 13 "calf" ""
            ]
        ]
            |> Grid.grid []


textField : Mdl -> Int -> String -> Result String num -> (String -> Msg) -> Html Msg
textField mdl i label value f =
    let
        content =
            case value of
                Ok num ->
                    Textfield.value (toString num)

                Err error ->
                    Textfield.error error
    in
        div []
            [ Textfield.render
                Mdl
                [ i ]
                mdl
                [ Textfield.label label
                , Textfield.floatingLabel
                , Textfield.text_
                , content
                , Options.onInput f
                ]
                []
            ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
