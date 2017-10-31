module HmtlView exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Ex3 exposing (..)
import String exposing (..)


    -- Model
type alias Model ={rna : Rna, checked : Bool}


initialModel : Model
initialModel = {rna = empty, checked = False}


    -- Update
type Msg =
    Const Base
    | Delete
    | Inverse
    | Kompl
    | UpLowerCase


newRna: Base -> Rna -> Rna
newRna b r =
    extend b r


upLowCase: String -> Bool -> String
upLowCase s m =
    if m then
        toLower s
    else
        toUpper s


update: Msg -> Model -> Model
update msg model =
    case msg of
        Const x ->
            {model | rna = newRna x model.rna}
        Delete ->
            {model | rna = empty }
        Inverse ->
            {model | rna = inverse model.rna}
        Kompl ->
            {model | rna = komp model.rna}
        UpLowerCase ->
            {model | checked = not model.checked}

    -- View
textStyle: Attribute msg
textStyle =
    style [("backgroundColor", "black"),("width","500px"),("color", "white"),("textAlign","right"),("fontSize","24px")]


buttonCss: Attribute msg
buttonCss =
    style [("width","100px"),("height","100px"),("color", "white"),("backgroundColor","green"),("fontSize", "20px")]


checkbox : Msg -> String -> Html Msg
checkbox msg title = label [ ] [input [type_ "checkbox", onClick msg ] [ ], text title ]

view : Model -> Html Msg
view model =
    div []
        [
        button [onClick (Const A), buttonCss] [text "Adenin"]
        , button [onClick (Const G), buttonCss] [text "Guanin"]
        , button [onClick (Const C), buttonCss] [text "Cytosin"]
        , button [onClick (Const U), buttonCss] [text "Uracil"]
        , button [onClick Delete, buttonCss] [text "Delete"]
        , button [onClick Inverse, buttonCss] [text "Inverse"]
        , button [onClick Kompl, buttonCss] [text "Komplementär"]
        , checkbox UpLowerCase "Change UpperLowerCase"
        , div [class "text", textStyle]
              [text (upLowCase (Ex3.toString model.rna) model.checked)]
         ]


main: Program Never Model Msg
main =
        beginnerProgram {model = initialModel, view= view, update = update}