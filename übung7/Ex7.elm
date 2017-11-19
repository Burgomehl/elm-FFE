module Ex7 exposing (..)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse exposing (..)

type alias Circle = {x: Float ,y: Float, radius: Float }

type alias Eye = {iris :Circle, border: Circle}

initialModel: (Model, Cmd Msg)
initialModel= ([{iris ={x = 80, y = 80, radius = 10},border ={x = 80, y = 80, radius = 40}}
                , {iris ={x = 160, y = 80, radius = 10},border ={x = 160, y = 80, radius = 40}}
                , {iris ={x = 80, y = 160, radius = 10},border ={x = 80, y = 160, radius = 40}}
                , {iris ={x = 160, y = 160, radius = 10},border ={x = 160, y = 160, radius = 40}}], Cmd.none)

type alias Model = List Eye

type Msg =
    Move Position

drawEye: Model -> List (Svg Msg)
drawEye xs =
     List.map (\e -> circle [cx (toString e.border.x), cy (toString e.border.y), r (toString e.border.radius), stroke "black", fill "white"][]) xs
     ++
     List.map (\e -> circle [cx (toString (e.iris.x)), cy (toString (e.iris.y)), r (toString e.iris.radius)][]) xs

calcMid: Model -> Position -> Model
calcMid ml p =
    case ml of
        [] ->
            []
        m::xs ->
            let
                rad = (m.border.radius - m.iris.radius)
                length = (sqrt(((toFloat p.y)- m.border.y)^2+((toFloat p.x)- m.border.x)^2))
                normx = ((toFloat p.x)- m.border.x)/length
                normy = ((toFloat p.y)- m.border.y)/ length
                px = rad*normx+m.border.x
                py = rad*normy+m.border.y
            in
            if length < rad then
                {m | iris = {x = toFloat p.x, y = toFloat p.y, radius = m.iris.radius}}::calcMid xs p
            else
                {m | iris = {x = px, y = py, radius = m.iris.radius}}::calcMid xs p

view: Model -> Html Msg
view e =
   div [] [
   svg [width "400", height "400", viewBox "0 0 400 400"]
   (drawEye e)]

update: Msg -> Model -> (Model, Cmd Msg)
update m e =
    case m of
        Move position ->
            (calcMid e position , Cmd.none)


sub: Model -> Sub Msg
sub e =
    Mouse.moves Move

main: Program Never Model Msg
main =
        program {init= initialModel, update= update, subscriptions = sub, view = view}