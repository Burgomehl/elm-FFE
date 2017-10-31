module HmtlView exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Ex4Rna exposing (..)
import String exposing (..)


    -- Model
type alias Model ={rna : Rna, checked : Bool, parsed : Bool, parsedRna : Rna, errorMsg : String}


initialModel : Model
initialModel = {rna = empty, checked = False, parsed = False, parsedRna = empty, errorMsg = ""}


    -- Update
type Msg =
    Const Base
    | Delete
    | Inverse
    | Kompl
    | UpLowerCase
    | Parse String
    | Transfer


newRna: Base -> Rna -> Rna
newRna b r =
    extend b r


upLowCase: String -> Bool -> String
upLowCase s m =
    if m then
        toLower s
    else
        toUpper s


parseStringToModel: String -> Model -> Model
parseStringToModel string model =
  case (parse string) of
    Ok v ->
      {model | parsedRna = v, parsed = True, errorMsg = "Erfolgreich geparsed"}
    Err d ->
      {model | errorMsg = d, parsed = False}


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
        Parse string ->
          parseStringToModel string model
        Transfer ->
          {model | rna = model.parsedRna}


    -- View
textStyle: Attribute msg
textStyle =
    style [("backgroundColor", "black"),("width","500px"),("color", "white"),("textAlign","right"),("fontSize","24px")]


buttonCss: Attribute msg
buttonCss =
    style [("width","100px"),("height","100px"),("color", "white"),("backgroundColor","blue"),("fontSize", "20px")]


checkbox : Msg -> String -> Html Msg
checkbox msg title = label [ ] [input [type_ "checkbox", onClick msg ] [ ], text title ]


parsedColor: Bool -> Attribute msg
parsedColor b =
  if b then
      style [("minWidth","100px"),("height","100px"),("color", "black"),("backgroundColor","green"),("fontSize", "20px")]
  else
      style [("minWidth","100px"),("height","100px"),("color", "black"),("backgroundColor","red"),("fontSize", "20px")]


errorMsgTextColor: Bool -> Attribute msg
errorMsgTextColor b =
  if b then
    textStyle
  else
    style [("backgroundColor", "black"),("width","500px"),("color", "red"),("textAlign","right"),("fontSize","24px")]



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
        , input [parsedColor model.parsed, onInput Parse] []
        , button [onClick Transfer, buttonCss, disabled (not model.parsed)][text "Transfer"]
        , div [class "text", errorMsgTextColor model.parsed]
              [text model.errorMsg]
        , div [class "text", textStyle]
              [text (upLowCase (Ex4Rna.toString model.rna) model.checked)]
         ]


main: Program Never Model Msg
main =
        beginnerProgram {model = initialModel, view= view, update = update}
