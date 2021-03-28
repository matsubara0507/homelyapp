module Model.DateTest exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Iso8601
import Test exposing (..)
import Time
import TimeZone
import Model.Date as Model

suite : Test
suite =
    describe "Model.Date"
        [ describe "lastMonth"
            [ fuzz (Fuzz.intRange 2 12) "not decrement Year when Month is between Feb and Dec" <|
                \month ->
                    let
                        model =
                            { year = 2020
                            , month = month
                            , zone = Time.utc
                            }
                    in
                    Expect.equal (Model.lastMonth model) ( 2020, month - 1 )
            , test "decrement Year when Month is Jan" <|
                \_ ->
                    let
                        model =
                            { year = 2020
                            , month = 1
                            , zone = Time.utc
                            }
                    in
                    Expect.equal (Model.lastMonth model) ( 2019, 12 )
            ]
        , describe "nextMonth"
            [ fuzz (Fuzz.intRange 1 11) "not increment Year when Month is between Jan and Nov" <|
                \month ->
                    let
                        model =
                            { year = 2020
                            , month = month
                            , zone = Time.utc
                            }
                    in
                    Expect.equal (Model.nextMonth model) ( 2020, month + 1 )
            , test "increment Year when Month is Dec" <|
                \_ ->
                    let
                        model =
                            { year = 2020
                            , month = 12
                            , zone = Time.utc
                            }
                    in
                    Expect.equal (Model.nextMonth model) ( 2021, 1 )
            ]
        , describe "syncYearMonth" <|
            let
                time =
                    Iso8601.toTime "2020-12-31T23:00:00Z"
            in
            [ test "override Year and Month" <|
                \_ ->
                    let
                        model =
                            { year = 2020
                            , month = 1
                            , zone = Time.utc
                            }
                    in
                    Expect.equal (Result.map (\t -> Model.syncYearMonth t model) time) (Ok { model | year = 2020, month = 12 })
            , test "override Year and Month with Asia/Tokyo" <|
                \_ ->
                    let
                        model =
                            { year = 2020
                            , month = 1
                            , zone = TimeZone.asia__tokyo ()
                            }
                    in
                    Expect.equal (Result.map (\t -> Model.syncYearMonth t model) time) (Ok { model | year = 2021, month = 1 })
            ]
        ]
