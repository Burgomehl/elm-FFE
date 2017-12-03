module Main exposing (..)

import Html exposing (Html)
import Color exposing (rgb)
import AFrame exposing (scene, entity)
import AFrame.Primitives exposing (sphere, box, cylinder, plane, sky)
import AFrame.Primitives.Attributes
    exposing
        ( rotation
        , position
        , radius
        , color
        , width
        , height
        , depth
        )


import Html exposing (..)
import AFrame exposing (..)
import AFrame.Primitives exposing (box)
import AFrame.Primitives.Attributes exposing (..)
import AFrame.Extra.Physics exposing (dynamicBody)
import Color exposing (rgb)


type alias Model =
    {area: PlayArea, snake:Snake}

type alias PlayArea =
    {width: Float
    ,depth: Float
    ,wallHeight: Float}

type alias Snake =
    {
    elements: List (Float,Float)
    }

type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( {area = {width= 100, depth = 100, wallHeight = 15}, snake={elements = [(0,0),(1,0),(2,0),(3,0)]}}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

generateWall: (Float ,Float ,Float) -> (Float, Float, Int) -> Html Msg
generateWall (x,y,z) (w,h,d)=
    box
        [ position x y z
        , radius 0.5
        , width w
        , height h
        , depth d
        , color (rgb 90 99 120)
        ]
        []

generateField: Model -> List (Html Msg)
generateField m =
    [plane
        [ rotation -90 0 0
        , width m.area.width
        , height m.area.depth
        , color (rgb 90 99 120)
        ]
        []
    , (generateWall (0,0,-m.area.depth/2) (m.area.width,m.area.wallHeight,1))
    , (generateWall (m.area.width/2,0,0) (1,m.area.wallHeight,round m.area.depth))
    , (generateWall (-m.area.width/2,0,0) (1,m.area.wallHeight,round m.area.depth))
    , (generateWall (0,0,m.area.depth/2) (m.area.width,m.area.wallHeight,1))
     ]

generateSphere: Float -> Float -> Html Msg
generateSphere x y =
     sphere
        [ position x 1.25 y
        , radius 1.25
        , color (rgb 240 173 0)
        ]
        []

generateSnake: Model -> List (Html Msg)
generateSnake m =
    List.map (\(x,y) -> (generateSphere x y)) m.snake.elements


view : Model -> Html Msg
view model =
        scene
                []
                ([ sky
                    []
                    []
                ]++(generateField model)
                ++(generateSnake model)
                )

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }