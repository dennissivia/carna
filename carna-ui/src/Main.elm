module Main exposing (..)

import String
import Regex
import String.Extra as StringExtra
import Result.Extra exposing (isOk)
import Maybe.Extra as MaybeExtra
import Html exposing (programWithFlags, div, text, span, h1, i, Html)
import Material
import Material.Table as Table
import Material.Elevation as Elevation
import Material.Card as Card
import Material.Dialog as Dialog
import Material.Color as MColor
import Color
import Material.Toggles as Toggles
import Material.Options exposing (Style, css, cs, id, nop)
import Material.Grid as Grid exposing (Device(..))
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
import Dom.Scroll
import Task
import WelcomeContent exposing (..)
import Markdown


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
    , bodyIndex : BodyIndexInput
    , bodyFatIndex : BodyFatIndex
    , bodyFatIndexSubmitted : Bool
    , bodyIndexSubmitted : Bool
    }


type alias OptionalValidatedInput a =
    Maybe (Result String a)


type alias BodyIndexInput =
    { age : OptionalValidatedInput Age
    , height : OptionalValidatedInput Float
    , weight : OptionalValidatedInput Float
    , waist : OptionalValidatedInput Float
    , hipSize : OptionalValidatedInput Float
    , gender : Maybe Gender
    , result : Maybe BodyIndexResult
    , isValid : Bool
    }


type alias BodyIndexValues =
    { age : Maybe Age
    , height : Maybe Float
    , weight : Maybe Float
    , waist : Maybe Float
    , hipSize : Maybe Float
    , gender : Maybe Gender
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
    { age : OptionalValidatedInput Age
    , height : OptionalValidatedInput Float
    , weight : OptionalValidatedInput Float
    , gender : Maybe Gender
    , skinFolds : SkinfoldInput
    , result : Maybe BodyFatIndexResult
    , isValid : Bool
    }


type alias BodyIndexResultRating =
    { bmi : BodyIndexSatisfaction
    , bai : BodyIndexSatisfaction
    , brocaIndex : BodyIndexSatisfaction
    , ponderalIndex : BodyIndexSatisfaction
    , surfaceArea : BodyIndexSatisfaction
    , whRatio : BodyIndexSatisfaction
    }


{-| used for getting /storing input and presenting errors
-}
type alias SkinfoldInput =
    { armpit : Maybe (Result String Float)
    , subscapular : Maybe (Result String Float) -- shoulder blade
    , chest : Maybe (Result String Float)
    , triceps : Maybe (Result String Float)
    , biceps : Maybe (Result String Float)
    , abdomen : Maybe (Result String Float)
    , iliacCrest : Maybe (Result String Float) -- Hip
    , thigh : Maybe (Result String Float)
    , calf : Maybe (Result String Float)
    }


type alias BodyFatIndexResult =
    { bodyFat3folds : Maybe Float
    , bodyFat4folds : Maybe Float
    , bodyFat7folds : Maybe Float
    , bodyFat9folds : Maybe Float
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
    | NoOp


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
        , mdl = Layout.setTabsWidth 1160 mdl
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
                EN
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

        RouteNotFound ->
            ""


changeUrl : Model -> Cmd msg
changeUrl =
    Navigation.newUrl << toUrl << .selectedTab


initialBodyIndex : BodyIndexInput
initialBodyIndex =
    { age = Nothing
    , height = Nothing
    , weight = Nothing
    , waist = Nothing
    , hipSize = Nothing
    , gender = Nothing
    , result = Nothing
    , isValid = False
    }


initialBodyFatIndex : BodyFatIndex
initialBodyFatIndex =
    { age = Nothing
    , height = Nothing
    , weight = Nothing
    , gender = Nothing
    , skinFolds =
        { armpit = Nothing
        , subscapular = Nothing
        , chest = Nothing
        , triceps = Nothing
        , biceps = Nothing
        , abdomen = Nothing
        , iliacCrest = Nothing
        , thigh = Nothing
        , calf = Nothing
        }
    , result = Nothing
    , isValid = False
    }


primaryColor : MColor.Hue
primaryColor =
    MColor.Teal


accentColor : MColor.Hue
accentColor =
    MColor.DeepOrange


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

                scrollCmd =
                    Task.attempt (always NoOp) (Dom.Scroll.toBottom "elm-mdl-layout-main")
            in
                { model | bodyIndexSubmitted = True, bodyIndex = newBodyIndex } ! [ scrollCmd ]

        NoOp ->
            model ! []

        BodyFatIndexSubmit ->
            let
                newBodyFatIndexResult =
                    calculateBodyFatIndexResult model.bodyFatIndex

                oldBodyFatIndex =
                    model.bodyFatIndex

                newBodyFatIndex =
                    { oldBodyFatIndex | result = newBodyFatIndexResult }

                scrollCmd =
                    Task.attempt (always NoOp) (Dom.Scroll.toBottom "elm-mdl-layout-main")
            in
                { model | bodyFatIndexSubmitted = True, bodyFatIndex = newBodyFatIndex } ! [ scrollCmd ]

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


updateBodyIndex : BodyIndexInput -> BodyIndexMsg -> BodyIndexInput
updateBodyIndex bodyIndex msg =
    let
        newBodyIndex =
            case msg of
                SetAge newAge ->
                    { bodyIndex | age = (Just <| validateAge newAge) }

                SetHeight newHeight ->
                    { bodyIndex | height = (Just <| validateHeight newHeight) }

                SetWeight newWeight ->
                    { bodyIndex | weight = (Just <| validateWeight newWeight) }

                SetWaist newWaist ->
                    { bodyIndex | waist = (Just <| validateWaist newWaist) }

                SetHip newHip ->
                    { bodyIndex | hipSize = (Just <| validateHip newHip) }

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
                    { bodyFatIndex | age = (Just <| validateAge newAge) }

                SetBfiHeight height ->
                    { bodyFatIndex | height = (Just <| validateHeight height) }

                SetBfiWeight weight ->
                    { bodyFatIndex | weight = (Just <| validateAge weight) }

                SetBfiGender gender ->
                    { bodyFatIndex | gender = Just gender }

                SetBfiSkinfold skinfoldMsg ->
                    { bodyFatIndex | skinFolds = updateSkinFolds bodyFatIndex.skinFolds skinfoldMsg }
    in
        { newBodyFatIndex | isValid = validateBodyFatIndex newBodyFatIndex }


updateSkinFolds : SkinfoldInput -> SkinfoldMsg -> SkinfoldInput
updateSkinFolds skinFolds msg =
    case msg of
        SetChest value ->
            { skinFolds | chest = Just <| validateFloat value }

        SetArmpit value ->
            { skinFolds | armpit = Just <| validateFloat value }

        SetSubscapular value ->
            { skinFolds | subscapular = Just <| validateFloat value }

        SetTriceps value ->
            { skinFolds | triceps = Just <| validateFloat value }

        SetBiceps value ->
            { skinFolds | biceps = Just <| validateFloat value }

        SetAbdomen value ->
            { skinFolds | abdomen = Just <| validateFloat value }

        SetIliacCrest value ->
            { skinFolds | iliacCrest = Just <| validateFloat value }

        SetThigh value ->
            { skinFolds | thigh = Just <| validateFloat value }

        SetCalf value ->
            { skinFolds | calf = Just <| validateFloat value }


optionalToMaybe : OptionalValidatedInput a -> Maybe a
optionalToMaybe =
    MaybeExtra.join << Maybe.map Result.toMaybe


toSkinfoldValues : SkinfoldInput -> Skinfolds
toSkinfoldValues skinfoldInput =
    { armpit = optionalToMaybe skinfoldInput.armpit
    , subscapular = optionalToMaybe skinfoldInput.subscapular
    , chest = optionalToMaybe skinfoldInput.chest
    , triceps = optionalToMaybe skinfoldInput.triceps
    , biceps = optionalToMaybe skinfoldInput.biceps
    , abdomen = optionalToMaybe skinfoldInput.abdomen
    , iliacCrest = optionalToMaybe skinfoldInput.iliacCrest
    , thigh = optionalToMaybe skinfoldInput.thigh
    , calf = optionalToMaybe skinfoldInput.calf
    }


calculateBodyFatIndexResult : BodyFatIndex -> Maybe BodyFatIndexResult
calculateBodyFatIndexResult bfi =
    let
        skinfolds =
            toSkinfoldValues bfi.skinFolds

        age =
            optionalToMaybe bfi.age

        weight =
            optionalToMaybe bfi.weight

        gender =
            Maybe.withDefault GenderOther bfi.gender
    in
        case bfi.isValid of
            True ->
                Just
                    { bodyFat3folds = caliper3foldsJp skinfolds gender age
                    , bodyFat4folds = caliper4foldsNhca skinfolds age
                    , bodyFat7folds = caliper7foldsJp skinfolds gender age
                    , bodyFat9folds = caliper9foldsParillo skinfolds weight
                    }

            False ->
                Nothing


calculateBodyIndexResult : BodyIndexInput -> Maybe BodyIndexResult
calculateBodyIndexResult bodyIndexInput =
    let
        bodyIndexValues =
            toBodyIndexValues bodyIndexInput
    in
        case bodyIndexInput.isValid of
            True ->
                Just
                    { bmi = calculateBMI bodyIndexValues.weight bodyIndexValues.height
                    , bai = calculateBAI bodyIndexValues.hipSize bodyIndexValues.height
                    , brocaIndex = calculateBrocaIndex bodyIndexValues.gender bodyIndexValues.height
                    , ponderalIndex = calculatePonderalIndex bodyIndexValues.weight bodyIndexValues.height
                    , surfaceArea = calculateSkinSurfaceArea bodyIndexValues.weight bodyIndexValues.height
                    , whRatio = calculateWaistHipRatio bodyIndexValues.waist bodyIndexValues.hipSize
                    }

            False ->
                Nothing


toBodyIndexValues : BodyIndexInput -> BodyIndexValues
toBodyIndexValues input =
    { age = optionalToMaybe input.age
    , height = optionalToMaybe input.height
    , weight = optionalToMaybe input.weight
    , waist = optionalToMaybe input.waist
    , hipSize = optionalToMaybe input.hipSize
    , gender = input.gender
    }


{-| TODO: split classificatin and satisfaction conversion into two functions that are piped together
-}
classifyBodyIndex : BodyIndexResult -> Maybe Age -> Maybe Gender -> BodyIndexResultRating
classifyBodyIndex bodyIndexResult age maybeGender =
    let
        gender =
            Maybe.withDefault GenderOther maybeGender
    in
        { bmi = classificationToSatisfaction <| classifyBMI bodyIndexResult.bmi
        , bai = classificationToSatisfaction <| Just (classifyBAI bodyIndexResult.bai age gender)
        , brocaIndex = classificationToSatisfaction <| Just (classifyBrocaIndex bodyIndexResult.brocaIndex)
        , ponderalIndex = classificationToSatisfaction <| Just (classifyPonderalIndex bodyIndexResult.ponderalIndex)
        , surfaceArea = classificationToSatisfaction <| classifySurfaceArea bodyIndexResult.surfaceArea age gender
        , whRatio = classificationToSatisfaction <| Just (classifyWaistHipRatio bodyIndexResult.whRatio gender)
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


validateBodyIndex : BodyIndexInput -> Bool
validateBodyIndex bodyIndex =
    Maybe.map3 (\a b c -> List.all isOk [ a, b, c ])
        bodyIndex.age
        bodyIndex.height
        bodyIndex.weight
        -- bodyIndex.waist
        -- bodyIndex.hipSize
        |> Maybe.withDefault False


validateBodyFatIndex : BodyFatIndex -> Bool
validateBodyFatIndex bodyFatIndex =
    Maybe.map3 (\a b c -> List.all isOk [ a, b, c ])
        bodyFatIndex.age
        bodyFatIndex.height
        bodyFatIndex.weight
        |> Maybe.withDefault False


{-| We could prepend error information this way
Result.mapError ((++) "Age") << validateChainFloat
-}
validateAge : String -> Result String Age
validateAge =
    validateChainFloat


validateHeight : String -> Result String Float
validateHeight =
    validateChainFloat


validateWeight : String -> Result String Float
validateWeight =
    validateChainFloat


validateWaist : String -> Result String Float
validateWaist =
    validateChainFloat


validateHip : String -> Result String Float
validateHip =
    validateChainFloat


validateChainFloat : String -> Result String Float
validateChainFloat val =
    Ok val
        |> Result.andThen validatePresence
        |> Result.andThen validateFloat


{-| FIXME we can only return I18n.Key as Err if we
either convert parseFloat errors to Keys or if we
pass the locale directly into all functions...
-}
validateFloat : String -> Result String Float
validateFloat str =
    case Regex.contains (Regex.regex "^[+-]?[0-9]+([.][0-9]+)?$") str of
        True ->
            String.toFloat str

        False ->
            Err <| "is not a valid number"


validatePresence : String -> Result String String
validatePresence str =
    if not (StringExtra.isBlank str) then
        Ok str
    else
        Err <| "should not be empty"


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme primaryColor accentColor <|
        Layout.render Mdl
            model.mdl
            [ Layout.selectedTab model.selectedTab
            , Layout.onSelectTab SelectTab
            , Layout.fixedHeader

            -- , Layout.fixedTabs
            -- , Layout.fixedDrawer
            ]
            { header =
                [ Layout.row
                    [ Options.nop, css "transition" "height 333ms ease-in-out 0s" ]
                    [ Layout.title [] [ text "Carna" ]
                    , Layout.spacer
                    , Layout.navigation []
                        [ Layout.link
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
                        [ Layout.href "/#welcome", Options.onClick (Layout.toggleDrawer Mdl) ]
                        [ text "welcome" ]
                    , Layout.link
                        [ Layout.href "/#body-index", Options.onClick (Layout.toggleDrawer Mdl) ]
                        [ text "body-index" ]
                    , Layout.link
                        [ Layout.href "/#body-fat", Options.onClick (Layout.toggleDrawer Mdl) ]
                        [ text "body-fat" ]
                    , Layout.link
                        [ Layout.href "https://github.com/scepticulous/carna-ng" ]
                        [ text "github" ]
                    ]
                ]
            , tabs =
                ( [ text "Welcome", text "Body Index", text "Body Fat" ]
                , [ MColor.background (MColor.color primaryColor MColor.S400)
                  ]
                )
            , main = [ viewBody model ]
            }


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        0 ->
            div [ class "grid-wrap" ] [ viewWelcomePage model ]

        1 ->
            div [ class "grid-wrap" ] [ viewBodyIndexForm model ]

        2 ->
            div [ class "grid-wrap" ] [ viewBodyFatIndexForm model ]

        _ ->
            div [ class "grid-wrap" ] [ viewWelcomePage model ]


gridCell : List (Style a) -> List (Html a) -> Grid.Cell a
gridCell styling =
    Grid.cell <| List.concat [ styling ]


{-|

  - FIXME How can we configure grid cell size per card/content?
  - FIXME Do we need a pair of (card, grid-columns(Num))
-}
viewWelcomePage : Model -> Html Msg
viewWelcomePage model =
    let
        cardInfo =
            carnaInfo model.locale

        cardInfo2 =
            bmiInfo model.locale

        cardInfo3 =
            calipometrie model.locale

        cardInfo4 =
            caliperMethods model.locale
    in
        div []
            [ viewContentRow
                [ viewContentCard cardInfo
                , viewContentCard cardInfo2
                , viewContentCard cardInfo3
                , viewContentCard cardInfo4
                ]
            ]


viewContentRow : List (Html Msg) -> Html Msg
viewContentRow cards =
    let
        gridStyleSmallCell =
            [ Grid.size All 4 ]

        gridStyleMediumCell =
            [ Grid.size Phone 4, Grid.size Tablet 8, Grid.size Desktop 8 ]
    in
        List.map (\card -> gridCell gridStyleSmallCell [ div [] [ card ] ]) cards
            |> Grid.grid []


viewContentCard : CardContent -> Html Msg
viewContentCard cardData =
    Card.view
        [ cs "content-card"
        , Elevation.e16
        ]
        [ Card.title []
            [ Card.head [] [ text cardData.head ]
            , Card.subhead [] [ text (Maybe.withDefault "" cardData.subhead) ]
            ]
        , Card.text [ cs "content-card-body-wrap" ] [ (Markdown.toHtml [] cardData.content) ]
        , Card.actions [ Card.border, MColor.text MColor.white ] []
        ]


viewBodyIndexForm : Model -> Html Msg
viewBodyIndexForm model =
    let
        gridStyle =
            [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 6, Grid.size Grid.Desktop 5 ]
    in
        [ gridCell gridStyle
            [ div
                []
                [ viewBodyIndexGenderSelect model
                , textField2 model.mdl 0 (I18n.t model.locale I18n.Age) (model.bodyIndex.age) (BodyIndexChange << SetAge)
                , textField2 model.mdl 1 (I18n.t model.locale I18n.Height) (model.bodyIndex.height) (BodyIndexChange << SetHeight)
                , textField2 model.mdl 2 (I18n.t model.locale I18n.Weight) (model.bodyIndex.weight) (BodyIndexChange << SetWeight)
                , textField2 model.mdl 3 (I18n.t model.locale I18n.Waist) (model.bodyIndex.waist) (BodyIndexChange << SetWaist)
                , textField2 model.mdl 4 (I18n.t model.locale I18n.Hip) (model.bodyIndex.hipSize) (BodyIndexChange << SetHip)
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


viewResultCard : Html Msg -> Locale -> Html Msg
viewResultCard cardBody locale =
    Card.view
        [ cs "result-card"
        , id "result-card"
        , MColor.background (MColor.color primaryColor MColor.S800)
        , Elevation.e16
        ]
        [ Card.title []
            [ Card.head [ MColor.background (MColor.color primaryColor MColor.S800) ] []
            , Card.subhead [ MColor.text MColor.white ] [ text (I18n.t locale I18n.YourResultHeading) ]
            ]
        , Card.text [ cs "result-table-wrap" ] [ cardBody ]
        , Card.actions [ Card.border, MColor.text MColor.white ] []
        ]


viewBodyIndexResultCard : BodyIndexInput -> Locale -> Html Msg
viewBodyIndexResultCard bodyIndex locale =
    let
        content =
            if bodyIndex.isValid then
                viewBodyIndexResulTable bodyIndex locale
            else
                div [ class "invalid-result" ] [ text (I18n.t locale I18n.InvalidResultContent) ]
    in
        viewResultCard content locale


viewBodyFatIndexResultCard : BodyFatIndex -> Locale -> Html Msg
viewBodyFatIndexResultCard bodyFatIndex locale =
    let
        content =
            if bodyFatIndex.isValid then
                viewBodyFatIndexResultTable bodyFatIndex locale
            else
                div [] [ text (I18n.t locale I18n.InvalidResultContent) ]
    in
        viewResultCard content locale


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


viewBodyIndexResulTable : BodyIndexInput -> Locale -> Html Msg
viewBodyIndexResulTable bodyIndex locale =
    case bodyIndex.result of
        Nothing ->
            div [] []

        Just result ->
            let
                bodyIndexRating =
                    classifyBodyIndex result (Maybe.andThen Result.toMaybe bodyIndex.age) bodyIndex.gender

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
                maybeAge =
                    optionalToMaybe bodyFatIndex.age

                classification =
                    classifyBodyFatIndex result maybeAge bodyFatIndex.gender

                t_ =
                    I18n.t locale
            in
                viewResultTable locale
                    [ viewResultTableRow (t_ I18n.BodyFatMethod3Folds) (viewBodyFatValue result.bodyFat3folds) classification.threeFolds
                    , viewResultTableRow (t_ I18n.BodyFatMethod4Folds) (viewBodyFatValue result.bodyFat4folds) classification.fourFolds
                    , viewResultTableRow (t_ I18n.BodyFatMethod7Folds) (viewBodyFatValue result.bodyFat7folds) classification.sevenFolds
                    , viewResultTableRow (t_ I18n.BodyFatMethod9Folds) (viewBodyFatValue result.bodyFat9folds) classification.nineFolds
                    ]


{-| TODO: use Classification module instead
-}
classifyBodyFatIndex : BodyFatIndexResult -> Maybe Age -> Maybe Gender -> BodyFatIndexResultRating
classifyBodyFatIndex bfi age maybeGender =
    let
        gender =
            Maybe.withDefault GenderOther maybeGender
    in
        { threeFolds = classificationToSatisfaction <| classifyBodyFat gender age bfi.bodyFat3folds
        , fourFolds = classificationToSatisfaction <| classifyBodyFat gender age bfi.bodyFat4folds
        , sevenFolds = classificationToSatisfaction <| classifyBodyFat gender age bfi.bodyFat7folds
        , nineFolds = classificationToSatisfaction <| classifyBodyFat gender age bfi.bodyFat9folds
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
            [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 4, Grid.size Grid.Desktop 4 ]

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
            [ [ gridCell gridStyle
                    [ viewBodyFatIndexGenderSelect model
                    , textField2 model.mdl 0 (t_ I18n.Age) (bodyFatIndex.age) (BodyFatIndexChange << SetBfiAge)
                    , textField2 model.mdl 1 (t_ I18n.Height) (bodyFatIndex.height) (BodyFatIndexChange << SetBfiHeight)
                    , textField2 model.mdl 2 (t_ I18n.Weight) (bodyFatIndex.weight) (BodyFatIndexChange << SetBfiWeight)
                    , textField2 model.mdl 3 (t_ I18n.Chest) (skinFolds.chest) (skinfoldMsgFunc << SetChest)
                    , textField2 model.mdl 4 (t_ I18n.Subscapular) (skinFolds.subscapular) (skinfoldMsgFunc << SetSubscapular)
                    , textField2 model.mdl 5 (t_ I18n.Armpit) (skinFolds.armpit) (skinfoldMsgFunc << SetArmpit)
                    ]
              , gridCell gridStyle
                    [ div [] [ span [] [] ]
                    , textField2 model.mdl 6 (t_ I18n.Biceps) (skinFolds.biceps) (skinfoldMsgFunc << SetBiceps)
                    , textField2 model.mdl 7 (t_ I18n.Triceps) (skinFolds.triceps) (skinfoldMsgFunc << SetTriceps)
                    , textField2 model.mdl 8 (t_ I18n.Abdomen) (skinFolds.abdomen) (skinfoldMsgFunc << SetAbdomen)
                    , textField2 model.mdl 9 (t_ I18n.IliacCrest) (skinFolds.iliacCrest) (skinfoldMsgFunc << SetIliacCrest)
                    , textField2 model.mdl 10 (t_ I18n.Thigh) (skinFolds.thigh) (skinfoldMsgFunc << SetThigh)
                    , textField2 model.mdl 11 (t_ I18n.Calf) (skinFolds.calf) (skinfoldMsgFunc << SetCalf)
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


textField2 : Mdl -> Int -> String -> OptionalValidatedInput num -> (String -> Msg) -> Html Msg
textField2 mdl i label value f =
    let
        content =
            case value of
                Nothing ->
                    nop

                Just result ->
                    case result of
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
