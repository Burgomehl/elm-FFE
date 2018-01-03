module Main exposing (..)

import Color exposing (rgb)
import AFrame.Primitives exposing (sphere, box, cylinder, plane, sky)
import AFrame.Primitives.Light exposing (light)
import Html exposing (..)
import Html.Attributes exposing (disabled)
import AFrame exposing (..)
import AFrame.Primitives.Camera exposing (..)
import AFrame.Primitives.Attributes exposing (..)
import AFrame.Extra.Physics exposing (kinematicBody, staticBody, dynamicBody)
import AFrame.Animations exposing (..)
import AFrame.Primitives.Light exposing (..)
import Color exposing (rgb)
import Keyboard exposing (..)
import Time exposing (..)
import Snake exposing (..)
import PlayArea exposing (..)
import Random exposing (..)
import StartPage exposing (..)

init : (Model, Cmd Msg)
init =
    (Start,Cmd.none)


startGame: (GameModel)
startGame =
    {snake = [(2,2),(3,3),(4,4)], area = {width= 100, depth = 100, wallHeight = 15}, food=[(4,4), (8,8), (-4,-4), (-8,-8)], nextMoveDir = N, pause = True}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Start ->
            case msg of
                    Key keycode ->
                        case keycode of
                           32 -> --space
                                (Play startGame, Cmd.none)
                           _ -> (model, Cmd.none)
                    Next ->
                        (model, Cmd.none)
                    SpawnFood (x,y)->
                        (model, Cmd.none)
                    GeneratePosition ->
                        (model, Cmd.none)
        Play gameModel ->
            case msg of
                Key keycode ->
                    case keycode of
                       32 -> --space
                            (Play {gameModel | pause = not gameModel.pause}, Cmd.none)
                       65 -> -- a
                            case gameModel.nextMoveDir of
                                N ->
                                    (Play {gameModel | nextMoveDir = W}, Cmd.none)
                                E ->
                                    (Play {gameModel | nextMoveDir = N}, Cmd.none)
                                S ->
                                    (Play {gameModel | nextMoveDir = E}, Cmd.none)
                                W ->
                                    (Play {gameModel | nextMoveDir = S}, Cmd.none)
                       68 -> -- d
                            case gameModel.nextMoveDir of
                                N ->
                                    (Play {gameModel | nextMoveDir = E}, Cmd.none)
                                E ->
                                    (Play {gameModel | nextMoveDir = S}, Cmd.none)
                                S ->
                                    (Play {gameModel | nextMoveDir = W}, Cmd.none)
                                W ->
                                    (Play {gameModel | nextMoveDir = N}, Cmd.none)
                       _ ->
                            (Play gameModel, Cmd.none)
                Next ->
                    calcNextPos gameModel
                SpawnFood (x,y)->
                    (Play {gameModel| food = (x,y)::gameModel.food},Cmd.none)
                GeneratePosition ->
                    (Play gameModel, generate SpawnFood (randomPoint gameModel))

randomPoint : GameModel -> Generator (Float,Float)
randomPoint m =
    pair (float (-m.area.width/2+2) (m.area.depth/2-2)) (float (-m.area.width/2+2) (m.area.depth/2-2))

nextPos: GameModel -> Float -> Float -> (Float, Float)
nextPos m x y =
    let
        moveD = 0.5
    in
    case m.nextMoveDir of
        N ->
            (x+moveD,y)
        E ->
            (x,y+moveD)
        S ->
            (x-moveD,y)
        W ->
            (x,y-moveD)

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


leftField: (Float, Float) -> GameModel -> Bool
leftField (x,y) model =
    not (model.area.width/2-1 >= x && x >= -model.area.width/2+1 && model.area.depth/2-1 >= y && y >= -model.area.depth/2+1)

hitSnake: (Float, Float) -> GameModel -> Bool
hitSnake (x,y) model =
    (model.snake |> List.filter (\(x1,y1) -> x == x1 && y == y1)
                |> List.length) > 1

calcNextPos: GameModel -> ( Model, Cmd Msg )
calcNextPos m =
            case List.head m.snake of
                Just (x,y)->
                    let
                        pos = nextPos m x y
                    in
                    if leftField (x,y) m then
                        (Start, Cmd.none)
                    else
                        if hitSnake (x,y) m then
                            (Start , Cmd.none)
                        else
                            if eating (x,y) m.food then
                                (Play {m | snake = (pos::m.snake), food = removeFood (x,y) m.food},Cmd.none)
                            else
                                (Play {m | snake = ((nextPos m x y)::List.take ((List.length m.snake)-1) m.snake)},Cmd.none)
                Nothing ->
                    (Play m, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
        case model of
            Start ->
                Keyboard.downs Key
            Play gameModel ->
                if gameModel.pause then
                    Keyboard.downs Key
                else
                    Sub.batch
                        [Keyboard.downs Key
                        , Time.every ( 0.02*Time.second) (\_-> Next)
                        , Time.every ( Time.second) (\_ -> GeneratePosition)
                        ]


generateSphere: Float -> Float -> Html Msg
generateSphere x y =
     entity [id "snakeE", position x 1.25 y
     ] [sphere
        [ src "https://raw.githubusercontent.com/aframevr/sample-assets/master/assets/images/space/earth_atmos_4096.jpg"
        , radius 1
        ]
        []]

generateSnake: GameModel -> List (Html Msg)
generateSnake m =
    List.map (\(x,y) -> (generateSphere x y)) m.snake


view : Model -> Html Msg
view model =
        case model of
            Start ->
                generatePage
            Play gameModel->
                scene
                        []
                        ([entity [id "snake"] (generateSnake gameModel)
                        , entity [id "food"] (generateFood gameModel)
                        , entity [id "enviorment"] [sky [
                                                    src "https://raw.githubusercontent.com/aframevr/sample-assets/master/assets/images/envmap/2294472375_24a3b8ef46_o.jpg" --"img/Park.jpg"
                                                    ][], light [AFrame.Primitives.Light.type_ Hemisphere][]]
                        , setCamera gameModel ]++(generateField gameModel))

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
        , color (rgb 255 0 0)
        , transparent True
        , opacity 1
        ][
            animation
            [attribute_ "rotation"
            , dur 10000
            , fill "forwards"
            , to "0 360 360"
            , AFrame.Animations.repeat "infinite"
            ]
            []
        ]]

generateFood: GameModel -> List (Html Msg)
generateFood m =
    List.map (\(x,y) -> generateFoodTile x y) m.food

getRotation: GameModel -> Attribute Msg
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

setCamera: GameModel -> Html Msg
setCamera m =
    case List.head m.snake of
        Just (x,y) ->
            camera [
                --lookControlsEnabled False
                wasdControlsEnabled False
                , position x 1.25 y
                , rotation 0 0 90
                , kinematicBody
            ][]
        Nothing ->
            camera [
                position 0 1.25 0
                ,getRotation m
            ][]