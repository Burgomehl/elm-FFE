module Main exposing (..)

import Color exposing (rgb)
import AFrame.Primitives exposing (sphere, box, cylinder, plane, sky, sound)
import AFrame.Primitives.Light exposing (light)
import Html exposing (..)
import Html.Attributes exposing (disabled, attribute, style)
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
    (startGame,Cmd.none)


startGame: (Model)
startGame =
    {snake = [(2,2),(3,3),(4,4)], area = {width= 100, depth = 100, wallHeight = 15}, food=[(4,4), (8,8), (-4,-4), (-8,-8)], nextMoveDir = N, pause = True, infoDisplay = False, isFog = False, isStats = False, speed = 2, cameraEnabled = True, points = 0}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
                Key keycode ->
                    case keycode of
                       65 -> -- a
                            case model.nextMoveDir of
                                N ->
                                    ({model | nextMoveDir = W}, Cmd.none)
                                E ->
                                    ({model | nextMoveDir = N}, Cmd.none)
                                S ->
                                    ({model | nextMoveDir = E}, Cmd.none)
                                W ->
                                    ({model | nextMoveDir = S}, Cmd.none)
                       68 -> -- d
                            case model.nextMoveDir of
                                N ->
                                    ({model | nextMoveDir = E}, Cmd.none)
                                E ->
                                    ({model | nextMoveDir = S}, Cmd.none)
                                S ->
                                    ({model | nextMoveDir = W}, Cmd.none)
                                W ->
                                    ({model | nextMoveDir = N}, Cmd.none)
                       _ ->
                            (model, Cmd.none)
                Next ->
                    calcNextPos model
                SpawnFood (x,y)->
                    ({model| food = (x,y)::model.food},Cmd.none)
                GeneratePosition ->
                    (model, generate SpawnFood (randomPoint model))
                Start ->
                    ({model | pause = not model.pause}, Cmd.none)
                ToggleInfo ->
                    ({model | infoDisplay = not model.infoDisplay}, Cmd.none)
                ToggleFog ->
                    ({model | isFog = not model.isFog}, Cmd.none)
                ToggleSpeed ->
                    ({model | speed = ((model.speed+1)%4)}, Cmd.none)
                ToggleStats ->
                    ({model | isStats = not model.isStats}, Cmd.none)
                ToggleCameraMode ->
                    ({model | cameraEnabled = not model.cameraEnabled}, Cmd.none)

randomPoint : Model -> Generator (Float,Float)
randomPoint m =
    pair (float (-m.area.width/2+2) (m.area.depth/2-2)) (float (-m.area.width/2+2) (m.area.depth/2-2))

nextPos: Model -> Float -> Float -> (Float, Float)
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


leftField: (Float, Float) -> Model -> Bool
leftField (x,y) model =
    not (model.area.width/2-1 >= x && x >= -model.area.width/2+1 && model.area.depth/2-1 >= y && y >= -model.area.depth/2+1)

hitSnake: (Float, Float) -> Model -> Bool
hitSnake (x,y) model =
    (model.snake |> List.filter (\(x1,y1) -> x == x1 && y == y1)
                |> List.length) > 1

calcNextPos: Model -> ( Model, Cmd Msg )
calcNextPos m =
            case List.head m.snake of
                Just (x,y)->
                    let
                        pos = nextPos m x y
                    in
                    if leftField (x,y) m then
                        (startGame, Cmd.none)
                    else
                        if hitSnake (x,y) m then
                            (startGame , Cmd.none)
                        else
                            if eating (x,y) m.food then
                                ({m | snake = (pos::m.snake), food = removeFood (x,y) m.food, points = m.points+1},Cmd.none)
                            else
                                ({m | snake = ((nextPos m x y)::List.take ((List.length m.snake)-1) m.snake)},Cmd.none)
                Nothing ->
                    (m, Cmd.none)

calcSpeed: Model -> Float
calcSpeed m =
    case m.speed of
        0 ->
            0.08
        1 ->
            0.04
        2 ->
            0.02
        3 ->
            0.01
        _ ->
            1

subscriptions : Model -> Sub Msg
subscriptions model =
            if model.pause then
                Keyboard.downs Key
            else
                Sub.batch
                    [Keyboard.downs Key
                    , Time.every ( (calcSpeed model)*Time.second) (\_-> Next) --ursprünglich 0.02
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

generateSnake: Model -> List (Html Msg)
generateSnake m =
    List.map (\(x,y) -> (generateSphere x y)) m.snake

appendFog: Model -> List (Attribute msg) -> List (Attribute msg)
appendFog m l =
            if m.isFog then
                List.append [attribute "fog" "type:exponential; color: #AAA; density: 0.05"] l
           else
                l
appendStats: Model -> List (Attribute msg) -> List (Attribute msg)
appendStats m l =
            if m.isStats then
                List.append [attribute "stats" ""] l
            else
                l

sceneOptions: Model -> List (Attribute msg)
sceneOptions m =
    []
    |> appendFog m
    |> appendStats m

view : Model -> Html Msg
view model =
                scene
                        (sceneOptions model)
                        ([entity [id "snake", AFrame.Primitives.Attributes.sound "src: url(bennySpielsound2.mp3); autoplay: true; loop: true"] (generateSnake model)
                        , entity [id "food"] (generateFood model)
                        --, sound [id "sound", position 0 0 0, src "http://localhost:8000/AFrame/bennySpielsound2.mp3", autoplay True, loop True, refDistance 200, volume 50][]
                        , entity [id "enviorment"] [sky [
                                                    src "https://raw.githubusercontent.com/aframevr/sample-assets/master/assets/images/envmap/2294472375_24a3b8ef46_o.jpg" --"img/Park.jpg"
                                                    ][], light [AFrame.Primitives.Light.type_ Hemisphere][]]
                        , setCamera model
                        , generatePage model
                        , showPoints model
                        ]++(generateField model))

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
        -- animation hat zu starken lags bei größerer Menge an Schlangenelementen und Essen gefübrt!
           {- animation
            [attribute_ "rotation"
            , dur 10000
            , fill "forwards"
            , to "0 360 360"
            , AFrame.Animations.repeat "infinite"
            ]
            []-}
        ]]

generateFood: Model -> List (Html Msg)
generateFood m =
    List.map (\(x,y) -> generateFoodTile x y) m.food

getRotation: Model -> Attribute Msg
getRotation m =
    case m.nextMoveDir of
        N->
            rotation 0 270 0
        E ->
            rotation 0 180 0
        S ->
            rotation 0 90 0
        W ->
            rotation 0 0 0

isCameraEnabled: Model -> List (Attribute Msg)
isCameraEnabled m =
    if m.cameraEnabled then
        [lookControlsEnabled True]
    else
        [
        lookControlsEnabled False
        ,getRotation m
        ]

setCamera: Model -> Html Msg
setCamera m =
    case List.head m.snake of
        Just (x,y) ->
            camera ([
                wasdControlsEnabled False
                , position x 1.25 y
                , rotation 0 0 90
                , kinematicBody
            ]++isCameraEnabled m)[]
        Nothing ->
            camera [
                position 0 1.25 0
                ,getRotation m
            ][]