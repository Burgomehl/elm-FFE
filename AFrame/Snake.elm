module Snake exposing (..)

type Msg =
    Next



type alias Model = {snake: Position}

type alias Position = {x: Float, y: Float}