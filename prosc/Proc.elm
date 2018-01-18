module Proc exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Code exposing (setup,draw)
import Messages exposing (Msg)
import Model exposing (..)
import Color exposing (..)

initialModel: (Model, Cmd Msg)
initialModel= (draw {x=0,y=0,color=Color.black, elements=[], forms=[]},Cmd.none)

view: Model -> Html Msg
view m =
   div []
   m.elements

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

sub: Model -> Sub Msg
sub model =
    Sub.none

main: Program Never Model Msg
main =
        program {init= initialModel, update= update, subscriptions = sub, view = view}