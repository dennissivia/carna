module Main exposing (..)

import String
import Regex
import String.Extra as StringExtra
import Html exposing (programWithFlags, div, text, span, h1, i, Html)
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
import BodyIndexClassification exposing (classifyBMI, classifyBAI, classifyBrocaIndex, classifyPonderalIndex, classifyWaistHipRatio, classifySurfaceArea)
import Utils exposing (Gender(..), Classification(..), Age)
import BodyFatCalculation exposing (Skinfolds, caliper3foldsJp, caliper4foldsNhca, caliper7foldsJp, caliper9foldsParillo)
import BodyFatClassification exposing (..)
import Navigation exposing (Location)
import UrlParser exposing (Parser, QueryParser, top, (<?>), string, stringParam)
import Html.Attributes exposing (href, class, style, width)
import Html.Events exposing (onWithOptions)
import Json.Decode as Decode


type alias Flags =
    { userLanguage : String }


{-| Gender, Age Height and Weight should be stored in the model, since it is genric enough
-}
type alias Model =
    { count : Int
    , mdl : Material.Model
    , selectedTab : Int
    , route : Route
    , userLanguage : String
    , bodyIndex : BodyIndex
    , bodyFatIndex : BodyFatIndex
    , bodyFatIndexSubmitted : Bool
    , bodyIndexSubmitted : Bool
    }


type alias BodyIndex =
    { age : Result String Age
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


type alias BodyFatIndex =
    { age : Result String Age
    , height : Result String Float
    , weight : Result String Float
    , gender : Maybe Gender
    , skinFolds : Skinfolds
    , result : Maybe BodyFatIndexResult
    , isValid : Bool
    }


type alias BodyFatIndexResult =
    { bodyFat1 : Maybe Float
    , bodyFat2 : Maybe Float
    , bodyFat3 : Maybe Float
    , bodyFat4 : Maybe Float
    }


type alias BodyIndexResultRating =
    { bmi : BodyIndexSatisfaction
    , bai : BodyIndexSatisfaction
    , brocaIndex : BodyIndexSatisfaction
    , ponderalIndex : BodyIndexSatisfaction
    , surfaceArea : BodyIndexSatisfaction
    , whRatio : BodyIndexSatisfaction
    }


type alias BodyFatIndexResultRating =
    { threeFolds : BodyIndexSatisfaction
    , fourFolds : BodyIndexSatisfaction
    , sevenFolds : BodyIndexSatisfaction
    , nineFolds : BodyIndexSatisfaction
    }


type Msg
    = SelectTab Int
    | BodyIndexSubmit
    | BodyFatIndexSubmit
    | Mdl (Material.Msg Msg)
    | BodyIndexChange BodyIndexMsg
    | BodyFatIndexChange BodyFatIndexMsg
    | UrlChange Location
    | NavigateTo Route


type BodyIndexMsg
    = SetAge String
    | SetHeight String
    | SetWeight String
    | SetWaist String
    | SetHip String
    | SetGender Gender


type BodyFatIndexMsg
    = SetBfiAge String
    | SetBfiHeight String
    | SetBfiWeight String
    | SetBfiGender Gender
    | SetBfiSkinfold SkinfoldMsg


type SkinfoldMsg
    = SetArmpit String
    | SetSubscapular String
    | SetChest String
    | SetTriceps String
    | SetBiceps String
    | SetAbdomen String
    | SetIliacCrest String
    | SetThigh String
    | SetCalf String


type Route
    = WelcomePage
    | BodyIndexPage
    | BodyFatPage
    | AboutPage
    | RouteNotFound


type BodyIndexSatisfaction
    = VerySatisfied
    | Satisfied
    | Neutral
    | Dissatisfied
    | VeryDissatisfied
    | SatisfactionUnknown


type alias Mdl =
    Material.Model


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    ( initialModel flags location, Cmd.none )


initialModel : Flags -> Location -> Model
initialModel flags location =
    let
        mdl =
            Material.model

        initialRoute =
            parseLocation location
    in
        { count = 0
        , mdl = Layout.setTabsWidth 200 mdl
        , route = initialRoute
        , selectedTab = routeToTabId initialRoute
        , userLanguage = flags.userLanguage
        , bodyIndex = initialBodyIndex
        , bodyIndexSubmitted = False
        , bodyFatIndex = initialBodyFatIndex
        , bodyFatIndexSubmitted = False
        }


toUrl : Int -> String
toUrl tabId =
    case tabId of
        0 ->
            "#welcome"

        1 ->
            "#body-index"

        2 ->
            "#body-fat"

        3 ->
            "#about"

        _ ->
            "#welcome"


routeToString : Route -> String
routeToString route =
    case route of
        WelcomePage ->
            "#welcome"

        BodyIndexPage ->
            "#body-index"

        BodyFatPage ->
            "#body-fat"

        AboutPage ->
            "#about"

        RouteNotFound ->
            ""


changeUrl : Model -> Cmd msg
changeUrl =
    Navigation.newUrl << toUrl << .selectedTab


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


initialBodyFatIndex : BodyFatIndex
initialBodyFatIndex =
    { age = Ok 27
    , height = Ok 165.5
    , weight = Ok 75
    , gender = Nothing
    , skinFolds =
        { armpit = Ok 20
        , subscapular = Ok 20
        , chest = Ok 20
        , triceps = Ok 20
        , biceps = Ok 20
        , abdomen = Ok 20
        , iliacCrest = Ok 20
        , thigh = Ok 20
        , calf = Ok 20
        }
    , result = Nothing
    , isValid = True
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
                newBodyIndexResult =
                    calculateBodyIndexResult model.bodyIndex

                oldBodyIndex =
                    model.bodyIndex

                newBodyIndex =
                    { oldBodyIndex | result = newBodyIndexResult }
            in
                { model | bodyIndexSubmitted = True, bodyIndex = newBodyIndex } ! []

        BodyFatIndexSubmit ->
            let
                newBodyFatIndexResult =
                    calculateBodyFatIndexResult model.bodyFatIndex

                oldBodyFatIndex =
                    model.bodyFatIndex

                newBodyFatIndex =
                    { oldBodyFatIndex | result = newBodyFatIndexResult }
            in
                { model | bodyFatIndexSubmitted = True, bodyFatIndex = newBodyFatIndex } ! []

        BodyIndexChange bodyIndexMessage ->
            let
                newBodyIndex =
                    updateBodyIndex model.bodyIndex bodyIndexMessage
            in
                { model | bodyIndex = newBodyIndex } ! []

        BodyFatIndexChange bodyFatIndexMessage ->
            let
                newBodyFatIndex =
                    updateBodyFatIndex model.bodyFatIndex bodyFatIndexMessage
            in
                { model | bodyFatIndex = newBodyFatIndex } ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model

        -- FIXME how do we get the route here?
        -- FIXME can we unify this with NavigateTo
        SelectTab num ->
            let
                newRoute =
                    tabToRoute num

                newModel =
                    { model | selectedTab = num, route = newRoute }

                newUrlCmd =
                    changeUrl newModel
            in
                Debug.log "SelectTab received " newModel ! [ newUrlCmd ]

        -- FIXME what do we have to do here?
        -- Handle
        UrlChange newLocation ->
            let
                newRoute =
                    parseLocation newLocation

                newTab =
                    routeToTabId newRoute
            in
                Debug.log ("UrlChange received with new location: " ++ (toString newLocation) ++ "and route: " ++ (toString newRoute) ++ " and new tab: " ++ (toString newTab)) { model | route = newRoute, selectedTab = newTab } ! []

        --  handle link clicks within the app
        -- FIXME what do we have to do here?
        NavigateTo newRoute ->
            let
                newTab =
                    routeToTabId newRoute

                newModel =
                    { model | route = newRoute, selectedTab = newTab }

                newUrlCmd =
                    Debug.log "change url cmd" <| changeUrl newModel
            in
                Debug.log "NavigateTo recieved " newModel ! [ newUrlCmd ]


updateBodyIndex : BodyIndex -> BodyIndexMsg -> BodyIndex
updateBodyIndex bodyIndex msg =
    let
        newBodyIndex =
            case msg of
                SetAge newAge ->
                    { bodyIndex | age = validateAge newAge }

                SetHeight newHeight ->
                    { bodyIndex | height = validateHeight newHeight }

                SetWeight newWeight ->
                    { bodyIndex | weight = validateWeight newWeight }

                SetWaist newWaist ->
                    { bodyIndex | waist = validateWaist newWaist }

                SetHip newHip ->
                    { bodyIndex | hipSize = validateHip newHip }

                SetGender gender ->
                    { bodyIndex | gender = Just gender }
    in
        { newBodyIndex | isValid = validateBodyIndex newBodyIndex }


updateBodyFatIndex : BodyFatIndex -> BodyFatIndexMsg -> BodyFatIndex
updateBodyFatIndex bodyFatIndex msg =
    let
        newBodyFatIndex =
            case msg of
                SetBfiAge newAge ->
                    { bodyFatIndex | age = validateAge newAge }

                SetBfiHeight height ->
                    { bodyFatIndex | height = validateHeight height }

                SetBfiWeight weight ->
                    { bodyFatIndex | weight = validateAge weight }

                SetBfiGender gender ->
                    { bodyFatIndex | gender = Just gender }

                SetBfiSkinfold skinfoldMsg ->
                    { bodyFatIndex | skinFolds = updateSkinFolds bodyFatIndex.skinFolds skinfoldMsg }
    in
        { newBodyFatIndex | isValid = validateBodyFatIndex newBodyFatIndex }


updateSkinFolds : Skinfolds -> SkinfoldMsg -> Skinfolds
updateSkinFolds skinFolds msg =
    case msg of
        SetChest value ->
            { skinFolds | chest = validateFloat "chest" value }

        SetArmpit value ->
            { skinFolds | armpit = validateFloat "armpit" value }

        SetSubscapular value ->
            { skinFolds | subscapular = validateFloat "subscapular" value }

        SetTriceps value ->
            { skinFolds | triceps = validateFloat "triceps" value }

        SetBiceps value ->
            { skinFolds | biceps = validateFloat "biceps" value }

        SetAbdomen value ->
            { skinFolds | abdomen = validateFloat "abdomen" value }

        SetIliacCrest value ->
            { skinFolds | iliacCrest = validateFloat "iliac" value }

        SetThigh value ->
            { skinFolds | thigh = validateFloat "thigh" value }

        SetCalf value ->
            { skinFolds | calf = validateFloat "calf" value }


calculateBodyFatIndexResult : BodyFatIndex -> Maybe BodyFatIndexResult
calculateBodyFatIndexResult bfi =
    case bfi.isValid of
        True ->
            Just
                { bodyFat1 = caliper3foldsJp bfi.skinFolds (Maybe.withDefault GenderOther bfi.gender) bfi.age
                , bodyFat2 = caliper4foldsNhca bfi.skinFolds bfi.age
                , bodyFat3 = caliper7foldsJp bfi.skinFolds (Maybe.withDefault GenderOther bfi.gender) bfi.age
                , bodyFat4 = caliper9foldsParillo bfi.skinFolds bfi.weight
                }

        False ->
            Nothing


calculateBodyIndexResult : BodyIndex -> Maybe BodyIndexResult
calculateBodyIndexResult bodyIndex =
    case bodyIndex.isValid of
        True ->
            Just
                { bmi = calculateBMI bodyIndex.weight bodyIndex.height
                , bai = calculateBAI bodyIndex.hipSize bodyIndex.height
                , brocaIndex = calculateBrocaIndex bodyIndex.gender bodyIndex.height
                , ponderalIndex = calculatePonderalIndex bodyIndex.weight bodyIndex.height
                , surfaceArea = calculateSkinSurfaceArea bodyIndex.weight bodyIndex.height
                , whRatio = calculateWaistHipRatio bodyIndex.waist bodyIndex.hipSize
                }

        False ->
            Nothing


{-| TODO: use Classification module instead
-}
classifyBodyIndex : BodyIndexResult -> Maybe Age -> Maybe Gender -> BodyIndexResultRating
classifyBodyIndex bodyIndexResult age gender =
    { bmi = classificationToSatisfaction <| classifyBMI bodyIndexResult.bmi
    , bai = classificationToSatisfaction <| Just (classifyBAI bodyIndexResult.bai age <| Maybe.withDefault GenderOther gender)
    , brocaIndex = classificationToSatisfaction <| Just (classifyBrocaIndex bodyIndexResult.brocaIndex)
    , ponderalIndex = classificationToSatisfaction <| Just (classifyPonderalIndex bodyIndexResult.ponderalIndex)
    , surfaceArea = classificationToSatisfaction <| classifySurfaceArea bodyIndexResult.surfaceArea age <| Maybe.withDefault GenderOther gender
    , whRatio = classificationToSatisfaction <| Just (classifyWaistHipRatio bodyIndexResult.whRatio <| Maybe.withDefault GenderOther gender)
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


validateBodyFatIndex : BodyFatIndex -> Bool
validateBodyFatIndex bodyFatIndex =
    Result.map5 (\_ _ _ _ _ -> True)
        bodyFatIndex.age
        bodyFatIndex.height
        bodyFatIndex.weight
        bodyFatIndex.skinFolds.chest
        bodyFatIndex.skinFolds.biceps
        |> Result.withDefault False


validateAge : String -> Result String Age
validateAge =
    validateChainFloat "Age"


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
                            [ Layout.href "https://github.com/scepticulous/carna-ng" ]
                            [ span [] [ text "github" ] ]
                        ]
                    ]
                ]
            , drawer =
                [ Layout.title [] [ text "Carna" ]
                , Layout.navigation
                    []
                    [ Layout.link
                        [ Layout.href "https://github.com/scepticulous/carna-ng" ]
                        [ text "github" ]
                    , Layout.link
                        [ Layout.href "/#welcome", Options.onClick (Layout.toggleDrawer Mdl) ]
                        [ text "welcome" ]
                    , Layout.link
                        [ Layout.href "/#body-index", Options.onClick (Layout.toggleDrawer Mdl) ]
                        [ text "body-index" ]
                    , Layout.link
                        [ Layout.href "/#body-fat", Options.onClick (Layout.toggleDrawer Mdl) ]
                        [ text "body-fat" ]
                    , Layout.link
                        [ Layout.href "/#about", Options.onClick (Layout.toggleDrawer Mdl) ]
                        [ text "about" ]
                    ]
                ]
            , tabs =
                ( [ text "Welcome", text "Body Index", text "Body Fat Calc", text "About" ]
                , [ MColor.background (MColor.color primaryColor MColor.S400)
                  ]
                )
            , main = [ viewBody model ]
            }


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        0 ->
            div [ class "grid-wrap" ] [ viewBodyIndexForm model ]

        1 ->
            div [ class "grid-wrap" ] [ viewBodyIndexForm model ]

        2 ->
            div [ class "grid-wrap" ] [ viewBodyFatIndexForm model ]

        3 ->
            div [ class "grid-wrap" ] [ viewBodyIndexForm model ]

        _ ->
            div [ class "grid-wrap" ] [ viewBodyIndexForm model ]


gridCell : List (Style a) -> List (Html a) -> Grid.Cell a
gridCell styling =
    Grid.cell <| List.concat [ styling ]


viewBodyIndexForm : Model -> Html Msg
viewBodyIndexForm model =
    let
        gridStyle =
            [ Grid.size Grid.All 12, Grid.size Grid.Desktop 5 ]
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
            [ Toggles.value <| hasGender model.bodyIndex.gender Female
            , Toggles.group "BodyIndexFormGender"
            , Toggles.ripple
            , Options.onToggle <| (BodyIndexChange << SetGender) Female
            ]
            [ text "Female" ]
        , Toggles.radio Mdl
            [ 1 ]
            model.mdl
            [ Toggles.value <| hasGender model.bodyIndex.gender Male
            , Toggles.group "BodyIndexFormGender"
            , Toggles.ripple
            , Options.onToggle <| (BodyIndexChange << SetGender) Male
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
                    classifyBodyIndex result (Result.toMaybe bodyIndex.age) bodyIndex.gender
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


viewBodyFatIndexResulTable : BodyFatIndex -> Html Msg
viewBodyFatIndexResulTable bodyFatIndex =
    case bodyFatIndex.result of
        Nothing ->
            div [] []

        Just result ->
            let
                classification =
                    classifyBodyFatIndex result (Result.toMaybe bodyFatIndex.age) bodyFatIndex.gender
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
                        [ viewBodyIndexResultRow "3 Falten" (viewBodyFatValue result.bodyFat1) classification.threeFolds
                        , viewBodyIndexResultRow "4 Falten" (viewBodyFatValue result.bodyFat2) classification.fourFolds
                        , viewBodyIndexResultRow "7 Falten" (viewBodyFatValue result.bodyFat3) classification.sevenFolds
                        , viewBodyIndexResultRow "9 Falten" (viewBodyFatValue result.bodyFat4) classification.nineFolds
                        ]
                    ]


viewBodyFatResultRow : String -> String -> BodyIndexSatisfaction -> Html Msg
viewBodyFatResultRow name value satisfaction =
    Table.tr
        []
        [ Table.td [] [ text name ]
        , Table.td [ Table.numeric ] [ text value ]
        , Table.td [ Table.numeric ] [ satisfactionIcon satisfaction ]
        ]


{-| TODO: use Classification module instead
-}
classifyBodyFatIndex : BodyFatIndexResult -> Maybe Age -> Maybe Gender -> BodyFatIndexResultRating
classifyBodyFatIndex bfi age gender =
    { threeFolds = classificationToSatisfaction <| classifyBodyFat (Maybe.withDefault GenderOther gender) age bfi.bodyFat1
    , fourFolds = classificationToSatisfaction <| classifyBodyFat (Maybe.withDefault GenderOther gender) age bfi.bodyFat2
    , sevenFolds = classificationToSatisfaction <| classifyBodyFat (Maybe.withDefault GenderOther gender) age bfi.bodyFat3
    , nineFolds = classificationToSatisfaction <| classifyBodyFat (Maybe.withDefault GenderOther gender) age bfi.bodyFat4
    }


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


viewBodyFatIndexGenderSelect : Model -> Html Msg
viewBodyFatIndexGenderSelect model =
    div
        [ class "gender-select" ]
        [ Toggles.radio Mdl
            [ 0 ]
            model.mdl
            [ Toggles.value <| hasGender model.bodyFatIndex.gender Female
            , Toggles.group "BodyFatIndexFormGender"
            , Toggles.ripple
            , Options.onToggle <| (BodyFatIndexChange << SetBfiGender) Female
            ]
            [ text "Female" ]
        , Toggles.radio Mdl
            [ 1 ]
            model.mdl
            [ Toggles.value <| hasGender model.bodyFatIndex.gender Male
            , Toggles.group "BodyFatIndexFormGender"
            , Toggles.ripple
            , Options.onToggle <| (BodyFatIndexChange << SetBfiGender) Male
            ]
            [ text "Male" ]
        ]


viewBodyFatIndexForm : Model -> Html Msg
viewBodyFatIndexForm model =
    let
        gridStyle =
            [ Grid.size Grid.All 12, Grid.size Grid.Desktop 4 ]

        bodyFatIndex =
            model.bodyFatIndex

        skinFolds =
            model.bodyFatIndex.skinFolds

        skinfoldMsgFunc =
            BodyFatIndexChange << SetBfiSkinfold
    in
        div []
            -- [ [ gridCell gridStyle [ viewBodyFatIndexGenderSelect model ] ] |> Grid.grid [ css "padding-bottom" "0px" ]
            [ [ gridCell gridStyle
                    [ viewBodyFatIndexGenderSelect model
                    , textField model.mdl 0 "Age" (bodyFatIndex.age) (BodyFatIndexChange << SetBfiAge)
                    , textField model.mdl 1 "Height" (bodyFatIndex.height) (BodyFatIndexChange << SetBfiHeight)
                    , textField model.mdl 2 "Weight" (bodyFatIndex.weight) (BodyFatIndexChange << SetBfiWeight)
                    , textField model.mdl 3 "Chest" (skinFolds.chest) (skinfoldMsgFunc << SetChest)
                    , textField model.mdl 4 "Shoulderblade" (skinFolds.subscapular) (skinfoldMsgFunc << SetSubscapular)
                    , textField model.mdl 5 "Armpid" (skinFolds.armpit) (skinfoldMsgFunc << SetArmpit)
                    ]
              , gridCell gridStyle
                    [ div [] [ span [] [] ]
                    , textField model.mdl 6 "Biceps" (skinFolds.biceps) (skinfoldMsgFunc << SetBiceps)
                    , textField model.mdl 7 "Triceps" (skinFolds.triceps) (skinfoldMsgFunc << SetTriceps)
                    , textField model.mdl 8 "Abdomen" (skinFolds.abdomen) (skinfoldMsgFunc << SetAbdomen)
                    , textField model.mdl 9 "Hip" (skinFolds.iliacCrest) (skinfoldMsgFunc << SetIliacCrest)
                    , textField model.mdl 10 "Thigh" (skinFolds.thigh) (skinfoldMsgFunc << SetThigh)
                    , textField model.mdl 11 "Calf" (skinFolds.calf) (skinfoldMsgFunc << SetCalf)
                    , Button.render Mdl
                        [ 5 ]
                        model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.ripple
                        , Options.onClick BodyFatIndexSubmit
                        ]
                        [ text "Calculate body fat" ]
                    ]
              , gridCell gridStyle
                    [ div []
                        [ if model.bodyFatIndexSubmitted then
                            div [] [ viewBodyFatIndexResultCard model.bodyFatIndex ]
                          else
                            div [] []
                        ]
                    ]
              ]
                |> Grid.grid []
            ]


viewBodyFatIndexResultCard : BodyFatIndex -> Html Msg
viewBodyFatIndexResultCard bodyFatIndex =
    Card.view
        [ css "height" "100%"
        , css "width" "320px"
        , MColor.background (MColor.color MColor.Brown MColor.S500)
        , Elevation.e16

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
                [ text "Your results" ]
            , Card.subhead []
                [ if bodyFatIndex.isValid then
                    viewBodyFatIndexResulTable bodyFatIndex
                  else
                    div [] [ text "invalid input" ]
                ]
            ]
        , Card.text [] [ text "We hope your are making progress. Great that you try to stay healty" ]
        , Card.actions [ Card.border, MColor.text MColor.white ] [ text "card actions" ]
        ]


viewBodyFatValue : Maybe Float -> String
viewBodyFatValue =
    Maybe.withDefault "N/A" << Maybe.map toString


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


{-| SPA internal links that can safely prevent defaults
-}
internalLink : Route -> Html Msg
internalLink route =
    let
        urlString =
            routeToString route
    in
        Html.a [ href urlString, onLinkClick (NavigateTo route) ] [ text urlString ]


{-| When clicking a link we want to prevent the default browser behaviour which is to load a new page.
So we use `onWithOptions` instead of `onClick`.
-}
onLinkClick : msg -> Html.Attribute msg
onLinkClick message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        onWithOptions "click" options (Decode.succeed message)


{-| reduce model with new location.
Sets current route and current tab in model
FIXME use me in update function
-}
updateCurrentRoute : Model -> Location -> Model
updateCurrentRoute model location =
    let
        newRoute =
            parseLocation location

        newTab =
            routeToTabId newRoute
    in
        { model | route = newRoute, selectedTab = newTab }


{-| parse initial browser location and UrlChange messages
-}
parseLocation : Location -> Route
parseLocation location =
    -- location
    -- |> UrlParser.parsePath routeParser |>
    Debug.log ("parse location for location: " ++ (toString location)) UrlParser.parseHash string location
        |> Maybe.map pathToRoute
        |> Maybe.withDefault RouteNotFound


{-| Parse the current route based on hash urls
-}



-- routeParser : UrlParser.Parser (Route -> a) a
-- routeParser =
--     UrlParser.oneOf
--         [ UrlParser.map WelcomePage UrlParser.top
--         , UrlParser.map WelcomePage (UrlParser.s "#welcome")
--         , UrlParser.map BodyIndexPage (UrlParser.s "#body-index")
--         , UrlParser.map BodyFatPage (UrlParser.s "#body-fat")
--         , UrlParser.map AboutPage (UrlParser.s "#about")
--         ]


location2TabID : Location -> Int
location2TabID location =
    let
        path =
            UrlParser.parseHash string location
    in
        Maybe.map lookupTabId path
            |> Maybe.withDefault 0


pathToRoute : String -> Route
pathToRoute path =
    let
        newRoute =
            case path of
                "welcome" ->
                    WelcomePage

                "body-index" ->
                    BodyIndexPage

                "body-fat" ->
                    BodyFatPage

                "about" ->
                    AboutPage

                _ ->
                    WelcomePage
    in
        Debug.log ("new route for path: " ++ (toString path)) newRoute


lookupTabId : String -> Int
lookupTabId path =
    case path of
        "#welcome" ->
            0

        "#body-index" ->
            1

        "#body-fat" ->
            2

        "#about" ->
            3

        _ ->
            0


routeToTabId : Route -> Int
routeToTabId route =
    case route of
        WelcomePage ->
            0

        BodyIndexPage ->
            1

        BodyFatPage ->
            2

        AboutPage ->
            3

        RouteNotFound ->
            0


tabToRoute : Int -> Route
tabToRoute tab =
    case tab of
        0 ->
            WelcomePage

        1 ->
            BodyIndexPage

        2 ->
            BodyFatPage

        3 ->
            AboutPage

        _ ->
            WelcomePage


hasGender : Maybe Gender -> Gender -> Bool
hasGender maybeGender otherGender =
    case maybeGender of
        Nothing ->
            False

        Just gender ->
            gender == otherGender


main : Program Flags Model Msg
main =
    Navigation.programWithFlags
        UrlChange
        { init = init
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
