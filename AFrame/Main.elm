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

init : (Model, Cmd Msg)
init =
    ({snake = {x = 2, y = 2}},Cmd.none)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            ({model| snake = {x = model.snake.x+1, y = model.snake.y}},Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
        Time.every ( Time.second) (\_-> Next)


view : Model -> Html Msg
view model =
{-verhält sich richtig merkwürdig ... Elemente bleiben an manchen Stellen hängen mauern und Boden bewegen sich mit ...-}
        scene
                []
                [entity [id "box", position model.snake.x 2 model.snake.y] [box [
                      color (rgb 10 10 10)
                      ,width 2
                      ,height 2
                      ,radius 0.5
                      ,depth 2][]
                      ]
                ,entity [id "wall", position 0 0 10] [box [
                                       color (rgb 10 40 10)
                                       ,width 20
                                       ,height 2
                                       ,radius 0.5
                                       ,depth 2][]
                                       ]
                ,entity [id "wall", position 0 0 -10] [box [
                                                        color (rgb 10 10 40)
                                                        ,width 20
                                                        ,height 2
                                                        ,radius 0.5
                                                        ,depth 2][]
                                                        ]
                ,entity [id "wall", position -10 0 0] [box [
                                       color (rgb 10 10 10)
                                       ,width 2
                                       ,height 2
                                       ,radius 0.5
                                       ,depth 20][]
                                       ]
                ,entity [id "wall", position 10 0 0] [box [
                                       color (rgb 10 10 10)
                                       ,width 2
                                       ,height 2
                                       ,radius 0.5
                                       ,depth 20][]
                                       ]
                ,entity [id "floor", position 0 0 0][ box [
                        color (rgb 40 40 40)
                        , height 1
                        , width 20
                        , height 0.1
                        , radius 0.5
                        , depth 20
                        , staticBody][]]
                , entity [] [sky [][], light [][]]]

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }