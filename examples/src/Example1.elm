module Example1 exposing (main)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Datetime
import Time
import Task


type alias Model =
  { model : Maybe Datetime.Model
  }

type Msg
  = OnMsg Datetime.Model
  | OnNow ( Time.Zone, Time.Posix )


main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


init : (Model, Cmd Msg)
init =
  ({ model = Nothing }
  , Task.perform OnNow (Task.map2 Tuple.pair Time.here Time.now)
  )


view : Model -> Html Msg
view model =
  div [ class "ex-datetime" ]
  [ Maybe.withDefault (text "") (model.model |> Maybe.map (Datetime.view OnMsg)) ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnMsg s ->
      ( {model | model = Just s}, Cmd.none )
    OnNow ( z, t ) ->
      ( { model | model = Just <| Datetime.init z t }, Cmd.none )
