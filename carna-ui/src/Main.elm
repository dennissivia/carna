module Main exposing (..)

import Array
import String
import String.Extra as StringExtra
import Html exposing (programWithFlags, div, text, h1, Html)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Elevation as Elevation
import Material.Card as Card
import Material.Color as Color
import Material.Toggles as Toggles
import Material.Options exposing (Style, css)
import Material.Grid as Grid
import Material.Textfield as Textfield
import Material.Layout as Layout
import Material.Scheme
import Material.Options as Options
import Material.Button as Button
import Material.Options exposing (css)


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
    { count = 0
    , mdl = Material.model
    , selectedTab = 0
    , userLanguage = flags.userLanguage
    , gender = Nothing
    , bodyIndex = initialBodyIndex
    , bodyIndexSubmitted = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BodyIndexSubmit ->
            model ! []

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

        -- When the `Mdl` messages come through, update appropriately.
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
    Material.Scheme.topWithScheme Color.Green Color.Purple <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.selectedTab model.selectedTab
            , Layout.onSelectTab SelectTab
            ]
            { header = [ h1 [ style [ ( "padding", "2rem" ) ] ] [ text "Carna - v2" ] ]
            , drawer = []
            , tabs = ( [ text "Body Index", text "Body Fat Calc", text <| "Browser language: " ++ model.userLanguage ], [ Color.background (Color.color Color.Teal Color.S400) ] )
            , main = [ viewBody model ]
            }


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        0 ->
            if model.bodyIndexSubmitted then
                div [] [ viewBodyIndexResultCard model.bodyIndex, viewBodyIndexForm model ]
            else
                div [] [ viewBodyIndexForm model ]

        1 ->
            div [] [ viewBodyFatForm model ]

        _ ->
            viewBodyIndexForm model


democell : Int -> List (Style a) -> List (Html a) -> Grid.Cell a
democell k styling =
    Grid.cell <| List.concat []


std : List (Style a) -> List (Html a) -> Grid.Cell a
std =
    democell 200


color : Int -> Style a
color k =
    Array.get ((k + 0) % Array.length Color.hues) Color.hues
        |> Maybe.withDefault Color.Teal
        |> flip Color.color Color.S500
        |> Color.background


viewBodyIndexForm : Model -> Html Msg
viewBodyIndexForm model =
    div
        [ style [ ( "padding", "3rem" ) ] ]
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
        |> Material.Scheme.top


viewBodyIndexResultCard : BodyIndex -> Html Msg
viewBodyIndexResultCard bodyIndex =
    Card.view
        [ css "height" "128px"
        , css "width" "128px"
        , Color.background (Color.color Color.Brown Color.S500)
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
                [ Color.text Color.white ]
                [ text "Hover here" ]
            ]
        ]


viewBodyFatForm : Model -> Html Msg
viewBodyFatForm model =
    let
        gridStyle =
            [ Grid.size Grid.All 4, color 5 ]
    in
        div [ style [ ( "padding", "3rem" ) ] ]
            [-- viewBodyFatPersonForm model
             -- , viewBodyFatSkinFoldForm model
            ]
            |> Material.Scheme.top


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
--             [ Grid.size Grid.All 4, color 5 ]
--     in
--         [ std gridStyle
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
--             [ Grid.size Grid.All 4, color 5 ]
--     in
--         [ std style
--             [ textField model.mdl 5 "Chest" ""
--             , textField model.mdl 6 "Shoulder" ""
--             , textField model.mdl 7 "Armpit" ""
--             ]
--         , std style
--             [ textField model.mdl 8 "Biceps" ""
--             , textField model.mdl 9 "Triceps" ""
--             , textField model.mdl 10 "Abdomen" ""
--             ]
--         , std style
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

                -- , Textfield.error
                --     ("Needs to be provided ")
                --     |> Options.when (String.isEmpty value)
                , Options.onInput f
                ]
                []
            ]



-- bodyIndexString : Result String value -> String
-- bodyIndexString result =
--     case result of
--         Err error ->
--             error
--         Ok val ->
--             toString val


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view

        -- Here we've added no subscriptions, but we'll need to use the `Mdl` subscriptions for some components later.
        , subscriptions = always Sub.none
        , update = update
        }
