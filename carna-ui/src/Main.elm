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
import I18n exposing (Locale(..))


type alias Flags =
    { userLanguage : String }


{-| Gender, Age Height and Weight should be stored in the model, since it is genric enough
-}
type alias Model =
    { count : Int
    , mdl : Material.Model
    , selectedTab : Int
    , route : Route
    , locale : Locale
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
        , locale = toLocale flags.userLanguage
        , bodyIndex = initialBodyIndex
        , bodyIndexSubmitted = False
        , bodyFatIndex = initialBodyFatIndex
        , bodyFatIndexSubmitted = False
        }


toLocale : String -> Locale
toLocale userLanguage =
    let
        locale =
            if Regex.contains (Regex.regex "de") userLanguage then
                DE
            else if Regex.contains (Regex.regex "en") userLanguage then
                DE
            else if Regex.contains (Regex.regex "fr") userLanguage then
                FR
            else
                DE
    in
        Debug.log ("converting " ++ userLanguage ++ " to ") locale


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
    { age = Ok 29
    , height = Ok 178
    , weight = Ok 65
    , waist = Ok 78
    , hipSize = Ok 98
    , gender = Just Female
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
                newModel =
                    updateCurrentRoute model newLocation
            in
                newModel ! []

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
                , textField model.mdl 0 (I18n.t model.locale I18n.Age) (model.bodyIndex.age) (BodyIndexChange << SetAge)
                , textField model.mdl 1 (I18n.t model.locale I18n.Height) (model.bodyIndex.height) (BodyIndexChange << SetHeight)
                , textField model.mdl 2 (I18n.t model.locale I18n.Weight) (model.bodyIndex.weight) (BodyIndexChange << SetWeight)
                , textField model.mdl 3 (I18n.t model.locale I18n.Waist) (model.bodyIndex.waist) (BodyIndexChange << SetWaist)
                , textField model.mdl 4 (I18n.t model.locale I18n.Hip) (model.bodyIndex.hipSize) (BodyIndexChange << SetHip)
                , Button.render Mdl
                    [ 5 ]
                    model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.onClick BodyIndexSubmit
                    ]
                    [ text (I18n.t model.locale I18n.CalculateBodyIndex) ]
                ]
            ]
        , gridCell gridStyle
            [ div []
                [ if model.bodyIndexSubmitted then
                    div [] [ viewBodyIndexResultCard model.bodyIndex model.locale ]
                  else
                    div [] []
                ]
            ]
        ]
            |> Grid.grid []


viewResultCard : Html Msg -> Html Msg
viewResultCard cardBody =
    Card.view
        [ cs "result-card"
        , MColor.background (MColor.color MColor.Brown MColor.S500)
        , Elevation.e16
        ]
        [ Card.title []
            [ Card.head [] []
            , Card.subhead [ MColor.text MColor.white ] [ text "Your results" ]
            ]
        , Card.text [ cs "result-table-wrap" ] [ cardBody ]
        , Card.actions [ Card.border, MColor.text MColor.white ] []
        ]


viewBodyIndexResultCard : BodyIndex -> Locale -> Html Msg
viewBodyIndexResultCard bodyIndex locale =
    let
        content =
            if bodyIndex.isValid then
                viewBodyIndexResulTable bodyIndex locale
            else
                div [] [ text (I18n.t locale I18n.InvalidResultContent) ]
    in
        viewResultCard content


viewBodyFatIndexResultCard : BodyFatIndex -> Locale -> Html Msg
viewBodyFatIndexResultCard bodyFatIndex locale =
    let
        content =
            if bodyFatIndex.isValid then
                viewBodyFatIndexResultTable bodyFatIndex locale
            else
                div [] [ text "invalid input" ]
    in
        viewResultCard content


viewGenderSelect : Model -> String -> Maybe Gender -> (Gender -> Msg) -> Html Msg
viewGenderSelect model groupName currentGender msg =
    div
        [ class "gender-select" ]
        [ Toggles.radio
            Mdl
            [ 0 ]
            model.mdl
            [ Toggles.value <| hasGender currentGender Female
            , Toggles.group groupName
            , Toggles.ripple
            , Options.onToggle <| msg Female
            ]
            [ text (I18n.t model.locale I18n.GenderOptionFemale) ]
        , Toggles.radio
            Mdl
            [ 1 ]
            model.mdl
            [ Toggles.value <| hasGender currentGender Male
            , Toggles.group groupName
            , Toggles.ripple
            , Options.onToggle <| msg Male
            ]
            [ text (I18n.t model.locale I18n.GenderOptionMale) ]
        ]


viewBodyIndexGenderSelect : Model -> Html Msg
viewBodyIndexGenderSelect model =
    viewGenderSelect model "BodyIndexFormGender" model.bodyIndex.gender (BodyIndexChange << SetGender)


viewBodyFatIndexGenderSelect : Model -> Html Msg
viewBodyFatIndexGenderSelect model =
    viewGenderSelect model "BodyFatIndexFormGender" model.bodyFatIndex.gender (BodyFatIndexChange << SetBfiGender)


viewResultTable : Locale -> List (Html Msg) -> Html Msg
viewResultTable locale tableBodyRows =
    Table.table [ cs "body-index-result-table" ]
        [ Table.thead []
            [ Table.tr []
                [ Table.th [] [ text (I18n.t locale I18n.BodyIndexResultColumnIndexName) ]
                , Table.th [] [ text (I18n.t locale I18n.BodyIndexResultColumnIndexValue) ]
                , Table.th [] [ text (I18n.t locale I18n.BodyIndexResultColumnIndexRating) ]
                ]
            ]
        , Table.tbody []
            tableBodyRows
        ]


viewResultTableRow : String -> String -> BodyIndexSatisfaction -> Html Msg
viewResultTableRow name value satisfaction =
    Table.tr
        []
        [ Table.td [] [ text name ]
        , Table.td [ Table.numeric ] [ text value ]
        , Table.td [ Table.numeric ] [ satisfactionIcon satisfaction ]
        ]


viewBodyIndexResulTable : BodyIndex -> Locale -> Html Msg
viewBodyIndexResulTable bodyIndex locale =
    case bodyIndex.result of
        Nothing ->
            div [] []

        Just result ->
            let
                bodyIndexRating =
                    classifyBodyIndex result (Result.toMaybe bodyIndex.age) bodyIndex.gender

                t_ =
                    I18n.t locale
            in
                viewResultTable locale
                    [ viewResultTableRow "BMI WHO" (toString result.bmi) bodyIndexRating.bmi
                    , viewResultTableRow "BAI" (toString result.bai) bodyIndexRating.bai
                    , viewResultTableRow "Broca Index" (toString result.brocaIndex) bodyIndexRating.brocaIndex
                    , viewResultTableRow "Ponderal Index" (toString result.ponderalIndex) bodyIndexRating.ponderalIndex
                    , viewResultTableRow "BSA" (toString result.surfaceArea) bodyIndexRating.surfaceArea
                    , viewResultTableRow "Waist-Hip ratio" (toString result.whRatio) bodyIndexRating.whRatio
                    ]


viewBodyFatIndexResultTable : BodyFatIndex -> Locale -> Html Msg
viewBodyFatIndexResultTable bodyFatIndex locale =
    case bodyFatIndex.result of
        Nothing ->
            div [] []

        Just result ->
            let
                classification =
                    classifyBodyFatIndex result (Result.toMaybe bodyFatIndex.age) bodyFatIndex.gender

                t_ =
                    I18n.t locale
            in
                viewResultTable locale
                    [ viewResultTableRow (t_ I18n.BodyFatMethod3Folds) (viewBodyFatValue result.bodyFat1) classification.threeFolds
                    , viewResultTableRow (t_ I18n.BodyFatMethod4Folds) (viewBodyFatValue result.bodyFat2) classification.fourFolds
                    , viewResultTableRow (t_ I18n.BodyFatMethod7Folds) (viewBodyFatValue result.bodyFat3) classification.sevenFolds
                    , viewResultTableRow (t_ I18n.BodyFatMethod9Folds) (viewBodyFatValue result.bodyFat4) classification.nineFolds
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


viewBodyFatIndexForm : Model -> Html Msg
viewBodyFatIndexForm model =
    let
        gridStyle =
            [ Grid.size Grid.Phone 12, Grid.size Grid.Tablet 5, Grid.size Grid.Desktop 4 ]

        bodyFatIndex =
            model.bodyFatIndex

        skinFolds =
            model.bodyFatIndex.skinFolds

        skinfoldMsgFunc =
            BodyFatIndexChange << SetBfiSkinfold

        t_ =
            I18n.t model.locale
    in
        div []
            -- [ [ gridCell gridStyle [ viewBodyFatIndexGenderSelect model ] ] |> Grid.grid [ css "padding-bottom" "0px" ]
            [ [ gridCell gridStyle
                    [ viewBodyFatIndexGenderSelect model
                    , textField model.mdl 0 (t_ I18n.Age) (bodyFatIndex.age) (BodyFatIndexChange << SetBfiAge)
                    , textField model.mdl 1 (t_ I18n.Height) (bodyFatIndex.height) (BodyFatIndexChange << SetBfiHeight)
                    , textField model.mdl 2 (t_ I18n.Weight) (bodyFatIndex.weight) (BodyFatIndexChange << SetBfiWeight)
                    , textField model.mdl 3 (t_ I18n.Chest) (skinFolds.chest) (skinfoldMsgFunc << SetChest)
                    , textField model.mdl 4 (t_ I18n.Subscapular) (skinFolds.subscapular) (skinfoldMsgFunc << SetSubscapular)
                    , textField model.mdl 5 (t_ I18n.Armpit) (skinFolds.armpit) (skinfoldMsgFunc << SetArmpit)
                    ]
              , gridCell gridStyle
                    [ div [] [ span [] [] ]
                    , textField model.mdl 6 (t_ I18n.Biceps) (skinFolds.biceps) (skinfoldMsgFunc << SetBiceps)
                    , textField model.mdl 7 (t_ I18n.Triceps) (skinFolds.triceps) (skinfoldMsgFunc << SetTriceps)
                    , textField model.mdl 8 (t_ I18n.Abdomen) (skinFolds.abdomen) (skinfoldMsgFunc << SetAbdomen)
                    , textField model.mdl 9 (t_ I18n.IliacCrest) (skinFolds.iliacCrest) (skinfoldMsgFunc << SetIliacCrest)
                    , textField model.mdl 10 (t_ I18n.Thigh) (skinFolds.thigh) (skinfoldMsgFunc << SetThigh)
                    , textField model.mdl 11 (t_ I18n.Calf) (skinFolds.calf) (skinfoldMsgFunc << SetCalf)
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
                            div [] [ viewBodyFatIndexResultCard model.bodyFatIndex model.locale ]
                          else
                            div [] []
                        ]
                    ]
              ]
                |> Grid.grid []
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


{-| parse initial browser location and UrlChange messages
-}
parseLocation : Location -> Route
parseLocation location =
    -- location
    -- |> UrlParser.parsePath routeParser |>
    Debug.log ("parse location for location: " ++ (toString location)) UrlParser.parseHash string location
        |> Maybe.map pathToRoute
        |> Maybe.withDefault RouteNotFound



-- Parse the current route based on hash urls
-- routeParser : UrlParser.Parser (Route -> a) a
-- routeParser =
--     UrlParser.oneOf
--         [ UrlParser.map WelcomePage UrlParser.top
--         , UrlParser.map WelcomePage (UrlParser.s "#welcome")
--         , UrlParser.map BodyIndexPage (UrlParser.s "#body-index")
--         , UrlParser.map BodyFatPage (UrlParser.s "#body-fat")
--         , UrlParser.map AboutPage (UrlParser.s "#about")
--         ]
-- {-| SPA internal links that can safely prevent defaults
-- NOTE not used at the moment
-- -}
-- internalLink : Route -> Html Msg
-- internalLink route =
--     let
--         urlString =
--             routeToString route
--     in
--         Html.a [ href urlString, onLinkClick (NavigateTo route) ] [ text urlString ]


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
