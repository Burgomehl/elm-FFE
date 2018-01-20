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

generatePage: Model -> Html Msg
generatePage m =
                div [menuStyle m.pause][
                    div [style [("padding","40px"),("fontSize","48px")]][text "SnakeVR"]
                    ,div [buttonDiv] [button [buttonStyle, onClick Start][text "Start"]]
                    ,div [buttonDiv] [button [buttonStyle, disabled True][text "Musik aus"]]
                    ,div [buttonDiv] [button [buttonStyle, disabled True][text "Mehrspieler?"]]
                    ,div [buttonDiv] [button [buttonStyle, onClick ToggleFog][text "Nebel an"]]
                    ,div [buttonDiv] [button [buttonStyle, onClick ToggleSpeed][text "Geschwindigkeit schnell"]]
                    ,div [buttonDiv] [button [buttonStyle, onClick ToggleStats][text "Performanceanzeige an"]]
                    ,div [buttonDiv] [button [buttonStyle, onClick ToggleInfo][text "Info"]]
                    ,div [outerDiv] [div [innerDiv] [text (infoText m)]]
                ]