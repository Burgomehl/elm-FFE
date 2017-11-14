module Ex7 exposing (..)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse exposing (..)

type alias Circle = {x: Float ,y: Float, radius: Float }

type alias Eye = {iris :Circle, border: Circle, value: String}

initialModel: (Model, Cmd Msg)
initialModel= ({iris ={x = 80, y = 80, radius = 10},border ={x = 80, y = 80, radius = 40}, value =""}, Cmd.none)

type alias Model = Eye

type Msg =
    Move Position

drawEye: Model -> List (Svg Msg)
drawEye e =
    [
    circle [cx (toString e.border.x), cy (toString e.border.y), r (toString e.border.radius), stroke "black", fill "white"][]
    , circle [cx (toString (e.iris.x)), cy (toString (e.iris.y)), r (toString e.iris.radius)][]
    ]

calcMid: Model -> Position -> Model
calcMid m p =
    let
        a = ((((toFloat p.y)- m.border.y)^2*(m.border.radius-m.iris.radius))/((toFloat p.x)-m.border.x))/2
        b = sqrt (((toFloat p.y)-m.border.y)^2+((toFloat p.x)-m.border.x)^2)
    in
    {m | iris = {x = m.border.x + a, y = m.border.y + b, radius = m.iris.radius}}

calcMid2: Model -> Position -> Model
calcMid2 m p =
    let
        rad = (m.border.radius - m.iris.radius)
        length = (sqrt(((toFloat p.y)- m.border.y)^2+((toFloat p.x)- m.border.x)^2))
        normx = ((toFloat p.x)- m.border.x)/length
        normy = ((toFloat p.y)- m.border.y)/ length
        px = rad*normx+m.border.x
        py = rad*normy+m.border.y
    in
    if length < rad then
        {m | iris = {x = toFloat p.x, y = toFloat p.y, radius = m.iris.radius}}
    else
        {m | iris = {x = px, y = py, radius = m.iris.radius}}

view: Model -> Html Msg
view e =
   div [] [
   svg []
   (drawEye e)

   , Html.text e.value]

update: Msg -> Model -> (Model, Cmd Msg)
update m e =
    case m of
        Move position ->
            (calcMid2 e position , Cmd.none)


sub: Model -> Sub Msg
sub e =
    Mouse.moves Move

main: Program Never Model Msg
main =
        program {init= initialModel, update= update, subscriptions = sub, view = view}