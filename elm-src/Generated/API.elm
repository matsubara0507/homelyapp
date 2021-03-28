module Generated.API exposing (..)

-- The following module comes from bartavelle/json-helpers

import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Set exposing (Set)
import String
import Url.Builder


type alias Expense =
    { amount : Int
    , date : String
    , description : String
    , labels : Set Int
    }


jsonDecExpense : Json.Decode.Decoder Expense
jsonDecExpense =
    Json.Decode.succeed (\pamount pdate pdescription plabels -> { amount = pamount, date = pdate, description = pdescription, labels = plabels })
        |> required "amount" Json.Decode.int
        |> required "date" Json.Decode.string
        |> required "description" Json.Decode.string
        |> required "labels" (decodeSet Json.Decode.int)


jsonEncExpense : Expense -> Value
jsonEncExpense val =
    Json.Encode.object
        [ ( "amount", Json.Encode.int val.amount )
        , ( "date", Json.Encode.string val.date )
        , ( "description", Json.Encode.string val.description )
        , ( "labels", encodeSet Json.Encode.int val.labels )
        ]


type alias Label =
    { name : String
    , description : String
    }


jsonDecLabel : Json.Decode.Decoder Label
jsonDecLabel =
    Json.Decode.succeed (\pname pdescription -> { name = pname, description = pdescription })
        |> required "name" Json.Decode.string
        |> required "description" Json.Decode.string


jsonEncLabel : Label -> Value
jsonEncLabel val =
    Json.Encode.object
        [ ( "name", Json.Encode.string val.name )
        , ( "description", Json.Encode.string val.description )
        ]


getApiExpenses : Int -> Int -> (Result Http.Error (Dict Int Expense) -> msg) -> Cmd msg
getApiExpenses query_year query_month toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    [ [ Just query_year
                            |> Maybe.map
                                (String.fromInt
                                    >> Url.Builder.string "year"
                                )
                      ]
                    , [ Just query_month
                            |> Maybe.map
                                (String.fromInt
                                    >> Url.Builder.string "month"
                                )
                      ]
                    ]
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "expenses"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (jsonDecDict Json.Decode.int jsonDecExpense)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postApiExpenses : Expense -> (Result Http.Error Int -> msg) -> Cmd msg
postApiExpenses body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "expenses"
                ]
                params
        , body =
            Http.jsonBody (jsonEncExpense body)
        , expect =
            Http.expectJson toMsg Json.Decode.int
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


deleteApiExpenses : Int -> (Result Http.Error () -> msg) -> Cmd msg
deleteApiExpenses query_expenseId toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    [ [ Just query_expenseId
                            |> Maybe.map
                                (String.fromInt
                                    >> Url.Builder.string "expenseId"
                                )
                      ]
                    ]
                )
    in
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "expenses"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectString
                (\x ->
                    case x of
                        Err e ->
                            toMsg (Err e)

                        Ok _ ->
                            toMsg (Ok ())
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getApiLabels : (Result Http.Error (Dict Int Label) -> msg) -> Cmd msg
getApiLabels toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "labels"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (jsonDecDict Json.Decode.int jsonDecLabel)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postApiLabels : Label -> (Result Http.Error Int -> msg) -> Cmd msg
postApiLabels body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "labels"
                ]
                params
        , body =
            Http.jsonBody (jsonEncLabel body)
        , expect =
            Http.expectJson toMsg Json.Decode.int
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


deleteApiLabels : Int -> (Result Http.Error () -> msg) -> Cmd msg
deleteApiLabels query_labelId toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    [ [ Just query_labelId
                            |> Maybe.map
                                (String.fromInt
                                    >> Url.Builder.string "labelId"
                                )
                      ]
                    ]
                )
    in
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "labels"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectString
                (\x ->
                    case x of
                        Err e ->
                            toMsg (Err e)

                        Ok _ ->
                            toMsg (Ok ())
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
