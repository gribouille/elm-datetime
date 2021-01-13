module Datetime exposing
    ( Model, getValue, init
    , phantom, view
    )

{-| Datetime component.


# Data

@docs Model, getValue, init


# View

@docs phantom, view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Regex


{-| Opaque type for model
-}
type Model
    = Model
        { date : String
        , time : String
        }


{-| Get the selected value (TODO: rename to `value`).
-}
getValue : Model -> String
getValue (Model m) =
    toString ( m.date, m.time )


{-| Initialize the component model.
-}
init : String -> Model
init date =
    let
        ( d, t ) =
            fromString date
    in
    Model { date = d, time = t }


{-| Component view.
-}
view : (Model -> msg) -> Model -> Html msg
view pipe (Model model) =
    div [ class "datetime" ]
        [ input
            [ class "date"
            , type_ "date"
            , onInput (\v -> pipe (Model { model | date = v }))
            , value model.date
            ]
            []
        , input
            [ class "time"
            , type_ "time"
            , onInput (\v -> pipe (Model { model | time = v }))
            , value model.time
            ]
            []
        ]


{-| Phantom view (for disabled state).
-}
phantom : Html msg
phantom =
    div [ class "datetime" ]
        [ input
            [ class "date is-disabled"
            , type_ "date"
            ]
            []
        , input
            [ class "time is-disabled"
            , type_ "time"
            ]
            []
        ]


fromString : String -> ( String, String )
fromString val =
    ( resolve regDate val, resolve regTime val )


toString : ( String, String ) -> String
toString ( h, t ) =
    h ++ "T" ++ t ++ ".00"


resolve : Regex.Regex -> String -> String
resolve r v =
    Maybe.withDefault "" <| Maybe.map .match <| List.head <| Regex.find r v


regTime : Regex.Regex
regTime =
    Maybe.withDefault Regex.never <| Regex.fromString "(\\d\\d:\\d\\d)"


regDate : Regex.Regex
regDate =
    Maybe.withDefault Regex.never <| Regex.fromString "^(\\d\\d\\d\\d-\\d\\d-\\d\\d)"
