module Ex8 exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)

initialModel: (Model, Cmd Msg)
initialModel= ({cPage = 1, comic = {number = 1, title = "", url= ""}}, loadComic 1)

type alias Model = {cPage: Int, comic: Comic}

url: Int -> String
url i = "http://xkcd.com/"++toString i++"/info.0.json"

type Msg =
    Next
    | Previous
    | First
    | Auto
    | Loaded (Result Http.Error Comic)

type alias Comic = {number : Int, title : String, url : String}

xkcdDecoder: Decoder Comic
xkcdDecoder =
    Json.Decode.map3 Comic
        (Json.Decode.field "num" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "img" Json.Decode.string)

errorDecoder: Decoder Comic
errorDecoder =
    Json.Decode.map3 Comic
                (Json.Decode.field "code" Json.Decode.int)
                (Json.Decode.field "title" Json.Decode.string)
                (Json.Decode.field "img" Json.Decode.string)

getData: Int -> Http.Request Comic
getData page =
    Http.get (url page) (oneOf [xkcdDecoder, errorDecoder])

loadComic: Int -> Cmd Msg
loadComic page =
    Http.send Loaded (getData page)
    --Http.send Loaded (Http.get (url m.cPage) xkcdDecoder)

checkbox : Msg -> String -> Html Msg
checkbox msg title = label [ ] [input [type_ "checkbox", onClick msg ] [ ], text title ]

view: Model -> Html Msg
view m =
   div [] [
   div[][
   button [onClick Previous][text "-1"]
   , button [onClick First][text "1"]
   , button [onClick Next][text "+1"]
   , checkbox Auto "Auto"
   ]
   , div [] [
   text (toString m.comic.number)
   , text m.comic.title
   , img [src m.comic.url] []
   ]
   ]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Next ->
            ({model | cPage = model.cPage+1}, loadComic (model.cPage+1))
        Previous ->
            ({model | cPage = model.cPage-1}, loadComic (model.cPage-1))
        First ->
            ({model | cPage = 1}, loadComic 1)
        Auto ->
            (model, Cmd.none)
        Loaded (Ok s) ->
            ({model | comic = s}, Cmd.none)
        Loaded (Err s) ->
            ({model | comic = {number = model.cPage, title= (toString s), url = "" }}, Cmd.none)

sub: Model -> Sub Msg
sub e =
    Sub.none

main: Program Never Model Msg
main =
        program {init= initialModel, update= update, subscriptions = sub, view = view}