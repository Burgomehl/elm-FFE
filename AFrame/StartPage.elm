module StartPage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Snake exposing (..)



menuStyle: Bool -> Attribute msg
menuStyle value =
    if value then
        style [("backgroundColor", "grey")
                ,("position","fixed")
                ,("left","200px")
                ,("top","100px")
                ,("right","200px")
                ,("bottom","100px")
                ,("zIndex","10000")
                ,("borderRadius","25px")
                ,("border","4px solid black")
                ,("textAlign","center")
                ]
    else
        style []

buttonStyle: Attribute msg
buttonStyle =
        style [("borderRadius","25px")
                ,("border","2px solid black")
                ,("width","200px")
                ,("height","40px")
                ,("-webkit-user-select","none")
                ,("-moz-user-select","none")
                ,("-ms-user-select","none")
                ,("user-select","none")
                ]

buttonDiv: Attribute msg
buttonDiv =
    style [("padding","1px")
            ,("position","relative")
            ]

infoText: Model -> String
infoText m =
    if m.infoDisplay then
        "SnakeVR ist ein Spiel, in dem der Spieler den Kopf einer Schlange steuert. Das Ziel des Spiels ist es so viel wie möglich zu essen. Die Schlange darf sich dabei nicht selber essen und nicht in die Wand beißen."
    else
        ""

innerDiv: Attribute msg
innerDiv =
    style [("fontSize","16px"),("width","200px"),("margin","0 auto")]

outerDiv: Attribute msg
outerDiv =
    style [("padding","20px"),("textAlign","center")]

isFog: Model -> String
isFog m =
    if m.isFog then
        "Nebel an"
    else
        "Nebel aus"

isStats: Model -> String
isStats m =
    if m.isStats then
        "Performanceanzeige an"
    else
        "Performanceanzeige aus"

isCameraMode: Model -> String
isCameraMode m =
    if m.cameraEnabled then
        "Kamerasteuerung an"
    else
        "Kamerasteuerung aus"

displaySpeed: Model -> String
displaySpeed m =
    case m.speed of
        0 ->
            "Geschwindigkeit sehr langsam"
        1 ->
            "Geschwindigkeit langsam"
        2 ->
            "Geschwindigkeit schnell"
        3 ->
            "Geschwindigkeit sehr schnell"
        _ ->
            "Speed Error "

transparentDiv:Bool -> Attribute msg
transparentDiv value =
                   if value then
                       style [("position","fixed")
                                ,("width","100%")
                                ,("height","100%")
                                ,("zIndex","9999")
                                ]
                   else
                       style []

generatePage: Model -> Html Msg
generatePage m =
            div [transparentDiv m.pause][
                div [menuStyle m.pause][
                    div [style [("padding","40px"),("fontSize","48px")]][text "SnakeVR"]
                    ,div [buttonDiv] [button [buttonStyle, onClick Start][text "Start"]]
                    ,div [buttonDiv] [button [buttonStyle, disabled True][text "Musik aus"]]
                    ,div [buttonDiv] [button [buttonStyle, disabled True][text "Mehrspieler?"]]
                    ,div [buttonDiv] [button [buttonStyle, onClick ToggleFog][text (isFog m)]]
                    ,div [buttonDiv] [button [buttonStyle, onClick ToggleSpeed][text (displaySpeed m)]]
                    ,div [buttonDiv] [button [buttonStyle, onClick ToggleStats][text (isStats m)]]
                    ,div [buttonDiv] [button [buttonStyle, onClick ToggleCameraMode][text (isCameraMode m)]]
                    ,div [buttonDiv] [button [buttonStyle, onClick ToggleInfo][text "Info"]]
                    ,div [outerDiv] [div [innerDiv] [text (infoText m)]]
                ]
            ]

showPoints: Model -> Html Msg
showPoints m =
    div [style [("top","10px")
                ,("right","10px")
                ,("zIndex","10000")
                ,("position","fixed")
                ,("-webkit-user-select","none")
                ,("-moz-user-select","none")
                ,("-ms-user-select","none")
                ,("user-select","none")
                ]][text ("Punkte: "++(toString m.points))]