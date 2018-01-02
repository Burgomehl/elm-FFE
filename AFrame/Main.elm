module Main exposing (..)

import Color exposing (rgb)
import AFrame.Primitives exposing (sphere, box, cylinder, plane, sky)
import AFrame.Primitives.Light exposing (light)
import Html exposing (..)
import AFrame exposing (..)
import AFrame.Primitives.Camera exposing (..)
import AFrame.Primitives.Attributes exposing (..)
import AFrame.Extra.Physics exposing (kinematicBody, staticBody, dynamicBody)
import AFrame.Animations exposing (..)
import Color exposing (rgb)
import Keyboard exposing (..)
import Time exposing (..)
import Snake exposing (..)
import PlayArea exposing (..)

init : (Model, Cmd Msg)
init =
    ({snake = [(2,2),(3,3),(4,4)], area = {width= 100, depth = 100, wallHeight = 15}, food=[(4,4), (8,8), (-4,-4), (-8,-8)], nextMoveDir = N},Cmd.none)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Key keycode ->
            case keycode of
                83 -> -- s
                    ({model | nextMoveDir = S}, Cmd.none)
                65 -> -- a
                    ({model | nextMoveDir = W}, Cmd.none)
                87 -> -- w
                    ({model | nextMoveDir = N}, Cmd.none)
                68 -> -- d
                    ({model | nextMoveDir = E}, Cmd.none)
                _ ->
                    (model, Cmd.none)
        Next ->
            calcNextPos model

nextPos: Model -> Float -> Float -> (Float, Float)
nextPos m x y =
    case m.nextMoveDir of
        N ->
            (x+1,y)
        E ->
            (x,y+1)
        S ->
            (x-1,y)
        W ->
            (x,y-1)

isInRange: (Float,Float) -> (Float, Float) -> Bool
isInRange (x,y) (x1,y1) =
    let
        range = 4
    in
    x1+range > x && x > x1-range && y1+range > y && y > y1-range

eating: (Float, Float) -> List (Float, Float) -> Bool
eating (x,y) l =
    (l
    |> List.filter (\(x1,y1) -> isInRange (x,y) (x1,y1))
    |> List.length) > 0

removeFood: (Float, Float) -> List (Float, Float) -> Position
removeFood (x,y) l =
        List.filter (\(x1,y1) -> not (isInRange (x,y) (x1,y1))) l


calcNextPos: Model -> ( Model, Cmd Msg )
calcNextPos m =

            case List.head m.snake of
                Just (x,y)->
                    let
                        pos = nextPos m x y
                    in
                    if eating (x,y) m.food then
                        ({m | snake = (pos::m.snake), food = removeFood (x,y) m.food},Cmd.none)
                    else
                        ({m | snake = ((nextPos m x y)::List.take ((List.length m.snake)-1) m.snake)},Cmd.none)
                Nothing ->
                    (m, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
        Sub.batch
            [Keyboard.downs Key
            , Time.every ( Time.second) (\_-> Next)
            ]


generateSphere: Float -> Float -> Html Msg
generateSphere x y =
     entity [id "snakeE", position x 1.25 y
     ] [sphere
        [ radius 1.25
        , color (rgb 240 173 150)
        ]
        []]

generateSnake: Model -> List (Html Msg)
generateSnake m =
    List.map (\(x,y) -> (generateSphere x y)) m.snake


view : Model -> Html Msg
view model =
{-verhält sich richtig merkwürdig ... Elemente bleiben an manchen Stellen hängen mauern und Boden bewegen sich mit ...-}
        scene
                []
                ([entity [id "snake"] (generateSnake model)
                , entity [id "food"] (generateFood model)
                , entity [id "enviorment"] [sky [][], light [][]]]++(generateField model))

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }

generateFoodTile: Float -> Float -> Html Msg
generateFoodTile x y =
     entity [id "foodTile", position x 1 y] [box
        [radius 0.5
        , width 1
        , height 1
        , depth 1
        , color (rgb 0 255 0)
        , transparent True
        , opacity 0.5
        ][
            animation
            [attribute_ "rotation"
            , dur 10000
            , fill "forwards"
            , to "0 360 360"
            , repeat "infinite"
            ]
            []
        ]]

generateFood: Model -> List (Html Msg)
generateFood m =
    List.map (\(x,y) -> generateFoodTile x y) m.food

getRotation: Model -> Attribute Msg
getRotation m =
    case m.nextMoveDir of
        N->
            rotation 90 0 0
        E ->
            rotation 180 0 0
        S ->
            rotation 270 0 0
        W ->
            rotation 0 0 0

setCamera: Model -> Html Msg
setCamera m =
    case List.head m.snake of
        Just (x,y) ->
            camera [
                --lookControlsEnabled False
                wasdControlsEnabled False
                , position x 10.25 y
                , rotation 0 0 90
                , kinematicBody
            ][]
        Nothing ->
            camera [
                position 0 1.25 0
                ,getRotation m
            ][]