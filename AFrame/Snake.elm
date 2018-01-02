module Snake exposing (..)

import Keyboard exposing (..)

type Msg =
    Next
    |Key KeyCode



type alias Model = {snake: Position, area: PlayArea, food: Position, nextMoveDir: Direction}

type alias Position = List (Float, Float)


type alias PlayArea =
    {width: Float
    ,depth: Float
    ,wallHeight: Float}

type Direction =
    N
    |E
    |W
    |S