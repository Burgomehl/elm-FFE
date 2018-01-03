module StartPage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Snake exposing (..)

generatePage: Html Msg
generatePage =
                div [][
                    div [][text "SnakeVR"]
                    ,div [] [button [][text "Einzelspieler"]]
                    ,div [] [button [disabled True][text "Mehrspieler"]]
                    ,div [] [button [disabled True][text "Musik aus"]]
                    ,div [] [button [disabled True][text "Skin"]]
                ]