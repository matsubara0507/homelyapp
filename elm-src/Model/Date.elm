module Model.Date exposing
    ( lastMonth
    , nextMonth
    , syncYearMonth
    )

import Time exposing (Month(..), Posix)


type alias M a =
    { a
        | year : Int
        , month : Int
        , zone : Time.Zone
    }


lastMonth : M a -> ( Int, Int )
lastMonth model =
    if model.month <= 1 then
        ( model.year - 1, 12 )

    else
        ( model.year, model.month - 1 )


nextMonth : M a -> ( Int, Int )
nextMonth model =
    if model.month >= 12 then
        ( model.year + 1, 1 )

    else
        ( model.year, model.month + 1 )


syncYearMonth : Posix -> M a -> M a
syncYearMonth t model =
    let
        month =
            case Time.toMonth model.zone t of
                Jan ->
                    1

                Feb ->
                    2

                Mar ->
                    3

                Apr ->
                    4

                May ->
                    5

                Jun ->
                    6

                Jul ->
                    7

                Aug ->
                    8

                Sep ->
                    9

                Oct ->
                    10

                Nov ->
                    11

                Dec ->
                    12
    in
    { model | year = Time.toYear model.zone t, month = month }
