module Ex2 exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
    -- Model
type alias Model =
    Int
initialModel : Model
initialModel =
    0
    -- Update
type Msg =
    Increase
    | Decrease
    | SetZero
    | Sum
    | Fak
update: Msg -> Model -> Model
update msg model =
    case msg of
        Increase ->
            model + 1
        Decrease ->
            model - 1
        SetZero ->
            0
        Sum ->
            sumN(model)
        Fak ->
            fakN(model)
    -- View
colorful: Attribute msg
colorful =
    style [("backgroundColor", "black"),("width","500px"),("height","90px"),("color", "white"),("textAlign","right"),("fontSize","24px")]
buttonCss: Attribute msg
buttonCss =
    style [("width","100px"),("height","100px"),("color", "white"),("backgroundColor","green"),("fontSize", "20px")]

view : Model -> Html Msg
view model =
    div []
        [ div [class "text", colorful]
            [text (toString model)]
        , button [onClick Increase, buttonCss] [text "+"]
        , button [onClick Decrease, buttonCss] [text "-"]
        , button [onClick SetZero, buttonCss] [text "0"]
        , button [onClick Sum, buttonCss, disabled (model <0)] [text "Sum"]
        , button [onClick Fak, buttonCss, disabled (model <0)] [text "Fak"]
            ]
main: Program Never Model Msg
main =
        beginnerProgram {model = initialModel, view= view, update = update}
sumN: Int -> Int
sumN n =
    if n<0 then
        -1
    else
        case n of
            0 ->
                0
            m ->
                n + sumN (n - 1)

fakN: Int -> Int
fakN n =
     if n<0 then
         -1
     else
         case n of
             1 ->
                 1
             m ->
                 n * fakN (n - 1)

