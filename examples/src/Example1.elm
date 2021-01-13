module Example1 exposing (main)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Datetime


type alias Model =
  { model : Datetime.Model
  }

type Msg
  = OnMsg Datetime.Model


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }


init : Model
init =
  { model = Datetime.init "2006-01-02T15:04:05.999999999Z07:00"
  }


view : Model -> Html Msg
view model =
  div [ class "ex-datetime" ]
  [ Datetime.view OnMsg model.model ]


update : Msg -> Model -> Model
update msg model =
  case msg of
    OnMsg s ->
      {model | model = s }
