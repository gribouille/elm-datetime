module Datetime exposing
    ( Model, get, init
    , phantom, view
    )

{-| Datetime component.


# Data

@docs Model, get, init


# View

@docs phantom, view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Parser exposing ((|.), (|=), Parser)
import Time exposing (Month(..), Posix, Zone)
import TimeExtra


{-| Opaque type for model
-}
type Model
    = Model
        { date : String
        , time : String
        , zone : Zone
        }


{-| Get the selected value.
-}
get : Model -> ( Zone, Posix )
get (Model m) =
    ( m.zone
    , Result.withDefault (Time.millisToPosix 0) <|
        Result.map2 (\( y, mo, d ) ( h, mi ) -> TimeExtra.fromYMDHM y mo d h mi)
            (Parser.run dateParser m.date)
            (Parser.run timeParser m.time)
    )


{-| Initialize the component model.
-}
init : Zone -> Posix -> Model
init z t =
    let
        date =
            String.fromInt (Time.toYear z t)
                ++ "-"
                ++ fmtMonth (Time.toMonth z t)
                ++ "-"
                ++ fmtString (Time.toDay z t)

        time =
            fmtString (Time.toHour z t)
                ++ ":"
                ++ fmtString (Time.toMinute z t)
    in
    Model { date = date, time = time, zone = z }


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


fmtString : Int -> String
fmtString i =
    let
        s =
            String.fromInt i
    in
    if String.length s == 1 then
        "0" ++ s

    else
        s


fmtMonth : Month -> String
fmtMonth month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


listMonths : List Month
listMonths =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


intToMonth : Int -> Month
intToMonth month =
    Maybe.withDefault Jan <| List.head <| List.drop month listMonths


dateParser : Parser ( Int, Month, Int )
dateParser =
    Parser.succeed (\a b c -> ( a, intToMonth b, c ))
        |= Parser.int
        |. Parser.oneOf [ Parser.symbol "-0", Parser.symbol "-" ]
        |= Parser.int
        |. Parser.oneOf [ Parser.symbol "-0", Parser.symbol "-" ]
        |= Parser.int


timeParser : Parser ( Int, Int )
timeParser =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.oneOf [ Parser.symbol ":0", Parser.symbol ":" ]
        |= Parser.int
