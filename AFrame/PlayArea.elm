module PlayArea exposing (..)
import AFrame.Primitives exposing (sphere, box, cylinder, plane, sky)
import AFrame exposing (..)
import AFrame.Primitives.Attributes exposing (..)
import AFrame.Extra.Physics exposing (..)
import Snake exposing (..)
import Html exposing (..)
import Color exposing (..)

generateWall: (Float ,Float ,Float) -> (Float, Float, Int) -> Html Msg
generateWall (x,y,z) (w,h,d)=
    entity [id "wall", position x y z] [box
        [ radius 0.5
        , width w
        , height h
        , depth d
        , color (rgb 90 99 120)
        -- ansonsten sind die Wände auch Durchsichtig
        , transparent False
        , opacity 1
        , staticBody
        ]
        []]

generateField: Model -> List (Html Msg)
generateField m =
    [entity [id "floor", position 0 0 0] [ plane
        [ rotation -90 0 0
        , width m.area.width
        , height m.area.depth
        , color (rgb 90 99 120)
        , staticBody
        ]
        []]
    , (generateWall (0,0,-m.area.depth/2) (m.area.width,m.area.wallHeight,1))
    , (generateWall (m.area.width/2,0,0) (1,m.area.wallHeight,round m.area.depth))
    , (generateWall (-m.area.width/2,0,0) (1,m.area.wallHeight,round m.area.depth))
    , (generateWall (0,0,m.area.depth/2) (m.area.width,m.area.wallHeight,1))
     ]