module Main exposing (..)

import Array
import Html exposing (programWithFlags, div, text, h1, Html)
import Html.Attributes exposing (href, class, style)
import Material
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


-- MODEL
-- You have to add a field to your model where you track the `Material.Model`.
-- This is referred to as the "model container"


type Gender
    = Male
    | Female


type alias Flags =
    { userLanguage : String }


type alias Model =
    { count : Int

    -- Boilerplate: model store for any and all Mdl components you use.
    , mdl : Material.Model
    , selectedTab : Int
    , userLanguage : String
    , gender : Maybe Gender
    }



-- `Material.model` provides the initial model


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags, Cmd.none )


initialModel : Flags -> Model
initialModel flags =
    { count = 0

    -- Boilerplate: Always use this initial Mdl model store.
    , mdl = Material.model
    , selectedTab = 0
    , userLanguage = flags.userLanguage
    , gender = Nothing
    }



-- ACTION, UPDATE
-- You need to tag `Msg` that are coming from `Mdl` so you can dispatch them
-- appropriately.


type Msg
    = SelectTab Int
    | BodyIndexSubmit
    | SelectGenderFemale
    | SelectGenderMale
    | Mdl (Material.Msg Msg)



-- Boilerplate: Msg clause for internal Mdl messages.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BodyIndexSubmit ->
            model ! []

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



-- VIEW


type alias Mdl =
    Material.Model


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
            viewBodyIndexForm model

        1 ->
            viewBodyFatForm model

        _ ->
            viewBodyIndexForm model


democell : Int -> List (Style a) -> List (Html a) -> Grid.Cell a
democell k styling =
    Grid.cell <| List.concat []



-- Grid.cell <| List.concat [ myStyle k, styling ]


std : List (Style a) -> List (Html a) -> Grid.Cell a
std =
    democell 200


color : Int -> Style a
color k =
    Array.get ((k + 0) % Array.length Color.hues) Color.hues
        |> Maybe.withDefault Color.Teal
        |> flip Color.color Color.S500
        |> Color.background


myStyle : Int -> List (Style a)
myStyle h =
    [ css "text-sizing" "border-box"
    , css "background-color" "#BDBDBD"
    , css "height" (toString h ++ "px")
    , css "padding-left" "8px"
    , css "padding-top" "4px"
    , css "color" "white"
    ]


viewBodyFatForm : Model -> Html Msg
viewBodyFatForm model =
    let
        gridStyle =
            [ Grid.size Grid.All 4, color 5 ]
    in
        div [ style [ ( "padding", "3rem" ) ] ]
            [ viewBodyFatPersonForm model
            , viewBodyFatSkinFoldForm model
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


viewBodyFatPersonForm : Model -> Html Msg
viewBodyFatPersonForm model =
    let
        gridStyle =
            [ Grid.size Grid.All 4, color 5 ]
    in
        [ std gridStyle
            [ viewBodyFatGenderForm model
            , textField model.mdl 0 "Age"
            , textField model.mdl 1 "Height"
            , textField model.mdl 2 "Weight"
            ]
        ]
            |> Grid.grid []


viewBodyFatSkinFoldForm : Model -> Html Msg
viewBodyFatSkinFoldForm model =
    let
        style =
            [ Grid.size Grid.All 4, color 5 ]
    in
        [ std style
            [ textField model.mdl 5 "Chest"
            , textField model.mdl 6 "Shoulder"
            , textField model.mdl 7 "Armpit"
            ]
        , std style
            [ textField model.mdl 8 "Biceps"
            , textField model.mdl 9 "Triceps"
            , textField model.mdl 10 "Abdomen"
            ]
        , std style
            [ textField model.mdl 11 "Hip"
            , textField model.mdl 12 "Quad/Thigh"
            , textField model.mdl 13 "calf"
            ]
        ]
            |> Grid.grid []


viewBodyIndexForm : Model -> Html Msg
viewBodyIndexForm model =
    div
        [ style [ ( "padding", "3rem" ) ] ]
        [ textField model.mdl 0 "Age"
        , textField model.mdl 1 "Height"
        , textField model.mdl 2 "Weight"
        , textField model.mdl 3 "Waist"
        , textField model.mdl 4 "Hip"
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


textField : Mdl -> Int -> String -> Html Msg
textField mdl i str =
    div []
        [ Textfield.render
            Mdl
            [ i ]
            mdl
            [ Textfield.label str
            , Textfield.floatingLabel
            , Textfield.text_

            -- Textfield.error
            -- ("Doesn't match " ++ rx)
            -- |> Options.when (not <| match model.str4 rx_)
            ]
            []
        ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view

        -- Here we've added no subscriptions, but we'll need to use the `Mdl` subscriptions for some components later.
        , subscriptions = always Sub.none
        , update = update
        }
