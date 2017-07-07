module Main exposing (..)

import String
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


type Gender
    = Male
    | Female


type alias Flags =
    { userLanguage : String }


type alias Model =
    { count : Int
    , mdl : Material.Model
    , selectedTab : Int
    , userLanguage : String
    , gender : Maybe Gender
    , bodyIndex : BodyIndex
    , bodyIndexSubmitted : Bool
    }


type alias BodyIndex =
    { age : Result String Int
    , height : Result String Float
    , weight : Result String Float
    , waist : Result String Float
    , hip : Result String Float
    , valid : Bool
    , result : Maybe BodyIndexResult
    }


type BodyIndexResult
    = BMI Float
    | BAI Float
    | BrocaIndex Float
    | PonderalIndex Float
    | SurfaceArea Float
    | WHRatio Float


type Msg
    = SelectTab Int
    | BodyIndexSubmit
    | SelectGenderFemale
    | SelectGenderMale
    | Mdl (Material.Msg Msg)
    | BodyIndexChange BodyIndexMsg


type BodyIndexMsg
    = SetAge String
    | SetHeight String
    | SetWeight String
    | SetWaist String
    | SetHip String


type alias Mdl =
    Material.Model


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags, Cmd.none )


initialBodyIndex : BodyIndex
initialBodyIndex =
    { age = Ok 28
    , height = Ok 165.5
    , weight = Ok 80
    , waist = Ok 80.2
    , hip = Ok 100.3
    , result = Nothing
    , valid = False
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
        , gender = Nothing
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
            { model | bodyIndexSubmitted = True } ! []

        BodyIndexChange bodyIndexMessage ->
            let
                newBodyIndex =
                    updateBodyIndex model.bodyIndex bodyIndexMessage
            in
                { model | bodyIndex = newBodyIndex } ! []

        SelectGenderMale ->
            { model | gender = Just Male } ! []

        SelectGenderFemale ->
            { model | gender = Just Female } ! []

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
                    { bodyIndex | hip = (validateHip newHip) }
    in
        { newBodyIndex | valid = validateBodyIndex newBodyIndex }


validateBodyIndex : BodyIndex -> Bool
validateBodyIndex bodyIndex =
    False


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
    case String.toFloat str of
        Ok val ->
            Ok val

        Err _ ->
            Err <| label ++ " is not a valid number"


validateInt : String -> String -> Result String Int
validateInt label str =
    case String.toInt str of
        Ok val ->
            Ok val

        Err _ ->
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
            [ Grid.size Grid.All 5, css "padding" "3rem" ]
    in
        [ gridCell gridStyle
            [ div
                []
                [ textField model.mdl 0 "Age" (model.bodyIndex.age) (BodyIndexChange << SetAge)
                , textField model.mdl 1 "Height" (model.bodyIndex.height) (BodyIndexChange << SetHeight)
                , textField model.mdl 2 "Weight" (model.bodyIndex.weight) (BodyIndexChange << SetWeight)
                , textField model.mdl 3 "Waist" (model.bodyIndex.waist) (BodyIndexChange << SetWaist)
                , textField model.mdl 4 "Hip" (model.bodyIndex.hip) (BodyIndexChange << SetHip)
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
    Table.table [ css "width" "100%", cs "body-index-result-table" ]
        [ Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "Body index" ]
                , Table.th [] [ text "Value" ]
                , Table.th [] [ text "Rating" ]
                ]
            ]
        , Table.tbody []
            [ viewBodyIndexResultRow "BMI WHO" (toString 22) Satisfied
            , viewBodyIndexResultRow "BAI" (toString 25.2) Satisfied
            , viewBodyIndexResultRow "Broca Index" (toString 75) Satisfied
            , viewBodyIndexResultRow "Ponderal Index" (toString 13.6) Satisfied
            , viewBodyIndexResultRow "Skin Surface Area" (toString 1.85) Satisfied
            , viewBodyIndexResultRow "Waist-Hip ratio" (toString 0.773) Satisfied
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


type BodyIndexSatisfaction
    = VerySatisfied
    | Satisfied
    | Neutral
    | Dissatisfied
    | VeryDissatisfied


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


-- viewBodyFatForm : Model -> Html Msg
-- viewBodyFatForm model =
--     let
--         gridStyle =
--             [ Grid.size Grid.All 4]
--     in
--         div [ style [ ( "padding", "3rem" ) ] ]
--             [ viewBodyFatPersonForm model
--               , viewBodyFatSkinFoldForm model
--             ]


viewBodyFatGenderForm : Model -> Html Msg
viewBodyFatGenderForm model =
    div
        []
        [ Toggles.radio Mdl
            [ 0 ]
            model.mdl
            [ Toggles.value <| hasGender model Female
            , Toggles.group "PersonGenderRadio"
            , Toggles.ripple
            , Options.onToggle SelectGenderFemale
            ]
            [ text "Female" ]
        , Toggles.radio Mdl
            [ 1 ]
            model.mdl
            [ Toggles.value <| hasGender model Male
            , Toggles.group "PersonGenderRadio"
            , Toggles.ripple
            , Options.onToggle SelectGenderMale
            ]
            [ text "Male" ]
        ]


hasGender : Model -> Gender -> Bool
hasGender model gender =
    case model.gender of
        Nothing ->
            False

        Just gender_ ->
            gender == gender_



-- viewBodyFatPersonForm : Model -> Html Msg
-- viewBodyFatPersonForm model =
--     let
--         gridStyle =
--             [ Grid.size Grid.All 4]
--     in
--         [ (gridCell) gridStyle
--             [ viewBodyFatGenderForm model
--             , textField model.mdl 0 "Age" ""
--             , textField model.mdl 1 "Height" ""
--             , textField model.mdl 2 "Weight" ""
--             ]
--         ]
--             |> Grid.grid []
-- viewBodyFatSkinFoldForm : Model -> Html Msg
-- viewBodyFatSkinFoldForm model =
--     let
--         style =
--             [ Grid.size Grid.All 4]
--     in
--         [ (gridCell) style
--             [ textField model.mdl 5 "Chest" ""
--             , textField model.mdl 6 "Shoulder" ""
--             , textField model.mdl 7 "Armpit" ""
--             ]
--         , (gridCell) style
--             [ textField model.mdl 8 "Biceps" ""
--             , textField model.mdl 9 "Triceps" ""
--             , textField model.mdl 10 "Abdomen" ""
--             ]
--         , (gridCell) style
--             [ textField model.mdl 11 "Hip" ""
--             , textField model.mdl 12 "Quad/Thigh" ""
--             , textField model.mdl 13 "calf" ""
--             ]
--         ]
--             |> Grid.grid []


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
