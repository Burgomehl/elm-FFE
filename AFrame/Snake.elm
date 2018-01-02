module Snake exposing (..)

import Keyboard exposing (..)

type Msg =
    Next
    |GeneratePosition
    |SpawnFood (Float, Float)
    |Key KeyCode



type alias Model = {snake: Position, area: PlayArea, food: Position, nextMoveDir: Direction, pause: Bool}

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