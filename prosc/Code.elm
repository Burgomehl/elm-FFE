module Code exposing (..)
import Html exposing (..)
import Messages exposing (Msg)
import Functions exposing (..)
import Model exposing (..)
import Color exposing (..)

draw: Model -> Model
draw m =
    m|>move (2,2)
     |>drawRect (4,4)
     |>move (20,20)
     |>drawRect (20,20)
     |>move(40,40)
     |>setColor Color.yellow
     |>drawRect(10,10)
     |>convert

setup: Int
setup =
    1