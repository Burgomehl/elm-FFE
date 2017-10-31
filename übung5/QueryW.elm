module HmtlView exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String exposing (..)
import List exposing (..)
import Dict exposing (..)


    -- Model
type alias Model = {dict: Dict String Int, currentView: Bool }


initialModel : Model
initialModel = {dict = Dict.fromList [("Ich bin über 50",0),--
    ("Ich habe ein Haustier",0),--
    ("Ich mag meinen Job",0),--
    ("Ich lebe im Weltall",0),--
    ("Ich lebe in Flensburg",0),--
    ("Ich habe Hunger",0)], currentView = True}


    -- Update
type Msg =
    Add String
    |ChangeView


increaseDictValue: Maybe Int -> Maybe Int
increaseDictValue m =
    case m of
        Just v -> Just (v+1)
        Nothing -> Nothing


update: Msg -> Model -> Model
update msg model =
    case msg of
        Add key->
          {model| dict = Dict.update key (\x -> increaseDictValue x) model.dict}
        ChangeView ->
          {model| currentView = not model.currentView}


createForm: Model -> List (Html Msg)
createForm  m =
    List.map (\(k,v) -> div [] [ button [onClick (Add k)][text k]])(Dict.toList m.dict)


createQueryResultForm: Model -> List (Html Msg)
createQueryResultForm m =
    List.map (\(k,v) -> div[] [text (k++": "++ toString v)]) (Dict.toList m.dict)


view : Model -> Html Msg
view model =
    if model.currentView then
        div [] ([
            div [] [
                button [onClick ChangeView][text "Auswertung"]
            ]
        ]++(createForm model))
    else
        queryResultView model


queryResultView : Model -> Html Msg
queryResultView model =
    div [] ([
        button [onClick ChangeView][text "Umfrage"]
    ]++(createQueryResultForm model))



main: Program Never Model Msg
main =
        beginnerProgram {model = initialModel, view= view, update = update}
