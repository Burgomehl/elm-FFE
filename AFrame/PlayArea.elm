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
        [ id "wallP"
        , radius 0.5
        , width w
        , height h
        , depth d
        -- ansonsten sind die Wände auch Durchsichtig
        , transparent False
        , opacity 1
        , staticBody
        , color (rgb 255 255 255)
        , src "https://raw.githubusercontent.com/aframevr/sample-assets/master/assets/images/bricks/brick_diffuse.jpg"
        , AFrame.Primitives.Attributes.repeat "5 1"
        ]
        []]

generateField: Model -> List (Html Msg)
generateField m =
    [entity [id "floor", position 0 0 0] [ plane
        [ rotation -90 0 0
        , width m.area.width
        , height m.area.depth
        , staticBody
        , src "https://raw.githubusercontent.com/aframevr/sample-assets/master/assets/images/terrain/grasslight-big.jpg"
        ]
        []]
    , (generateWall (0,0,-m.area.depth/2) (m.area.width,m.area.wallHeight,1))
    , (generateWall (m.area.width/2,0,0) (1,m.area.wallHeight,round m.area.depth))
    , (generateWall (-m.area.width/2,0,0) (1,m.area.wallHeight,round m.area.depth))
    , (generateWall (0,0,m.area.depth/2) (m.area.width,m.area.wallHeight,1))
     ]