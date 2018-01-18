module Functions exposing (..)
import Messages exposing (Msg)
import Html exposing (..)
import Model exposing (..)
import Collage exposing (collage,rect, toForm, filled, move)
import Element exposing (toHtml)
import Color exposing (..)

drawButton: Html Msg
drawButton =
    button [][text "Hallo"]

move: (Float,Float) -> Model -> Model
move (x1,y1) m =
    {m | x=x1, y=y1}

setColor: Color -> Model -> Model
setColor c m =
    {m| color = c}

drawRect: (Float, Float) -> Model -> Model
drawRect (x1,y1) m =
    {m | forms =  (Collage.move (m.x, m.y) (filled m.color (rect x1 y1)))::m.forms}

convert: Model -> Model
convert m =
    {m | elements = [(toHtml (collage 500 500 m.forms))]}