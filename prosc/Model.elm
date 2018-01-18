module Model exposing (..)
import Html exposing (..)
import Messages exposing (..)
import Collage exposing (Form)
import Color exposing (..)

type alias Model = {x: Float,y:Float, color:Color, elements: List (Html Msg), forms: List Form}