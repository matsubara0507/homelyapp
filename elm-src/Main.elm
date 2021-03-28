module Main exposing (main)

import Browser as Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, href, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Date as Model
import Set
import String.Extra as String
import Task
import Time exposing (Month(..), Posix)
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, top)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , expenses : Dict Int API.Expense
    , year : Int
    , month : Int
    , zone : Time.Zone
    , inputAmount : String
    , inputDay : String
    , inputDescription : String
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | FetchExpenses (Result Http.Error (Dict Int API.Expense))
    | CreateExpense API.Expense (Result Http.Error Int)
    | HereZone Time.Zone
    | Now Posix
    | Input InputExpenseColumn String
    | SubmitExpense


type InputExpenseColumn
    = ExpenseAmount
    | ExpenseDate
    | ExpenseDescription


init : {} -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    stepUrl url
        { key = key
        , expenses = Dict.empty
        , year = 0
        , month = 0
        , zone = Time.utc
        , inputAmount = ""
        , inputDay = ""
        , inputDescription = ""
        }


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            route (Parser.int </> Parser.int)
                (\y m -> ( y, m ))
    in
    case Parser.parse parser url of
        Nothing ->
            ( model, fetchNow )

        Just ( y, m ) ->
            fetchExpenses { model | year = y, month = m }


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


fetchExpenses : Model -> ( Model, Cmd Msg )
fetchExpenses model =
    ( model, API.getApiExpenses model.year model.month FetchExpenses )


fetchNow : Cmd Msg
fetchNow =
    Cmd.batch [ Task.perform HereZone Time.here, Task.perform Now Time.now ]


createExpense : Model -> ( Model, Cmd Msg )
createExpense model =
    case ( String.toInt model.inputAmount, String.toInt model.inputDay ) of
        ( Just amount, Just day ) ->
            let
                expense =
                    { amount = amount
                    , date =
                        String.join "-"
                            [ String.fromInt model.year
                                |> String.padLeft 4 '0'
                                |> String.right 4
                            , String.fromInt model.month
                                |> String.padLeft 2 '0'
                                |> String.right 2
                            , String.fromInt day
                                |> String.padLeft 2 '0'
                                |> String.right 2
                            ]
                    , description = model.inputDescription
                    , labels = Set.empty
                    }
            in
            ( model, API.postApiExpenses expense (CreateExpense expense) )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model

        FetchExpenses (Ok expenses) ->
            ( { model | expenses = expenses }, Cmd.none )

        FetchExpenses (Err _) ->
            ( model, Cmd.none )

        CreateExpense expense (Ok expenseId) ->
            ( { model | expenses = Dict.insert expenseId expense model.expenses }, Cmd.none )

        CreateExpense _ (Err _) ->
            ( model, Cmd.none )

        HereZone zone ->
            ( { model | zone = zone }, Cmd.none )

        Now t ->
            fetchExpenses (Model.syncYearMonth t model)

        Input ExpenseAmount s ->
            ( { model | inputAmount = s }, Cmd.none )

        Input ExpenseDate s ->
            ( { model | inputDay = s }, Cmd.none )

        Input ExpenseDescription s ->
            ( { model | inputDescription = s }, Cmd.none )

        SubmitExpense ->
            createExpense model


view : Model -> Browser.Document Msg
view model =
    { title = "Homely App"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ viewHeader model
    , viewExpenseInput model
    , viewExpenses model
    ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        ( ly, lm ) =
            Model.lastMonth model

        ( ny, nm ) =
            Model.nextMonth model
    in
    section [ class "hero is-primary" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text "Homely App" ]
                , p [ class "subtitle" ]
                    [ a [ href ("/" ++ String.fromInt ly ++ "/" ++ String.fromInt lm) ]
                        [ span [ class "icon is-medium" ]
                            [ i [ class "fas fa-angle-double-left" ] [] ]
                        ]
                    , text (String.fromInt model.year ++ "/" ++ String.fromInt model.month)
                    , a [ href ("/" ++ String.fromInt ny ++ "/" ++ String.fromInt nm) ]
                        [ span [ class "icon is-medium" ]
                            [ i [ class "fas fa-angle-double-right" ] [] ]
                        ]
                    ]
                ]
            ]
        ]


viewExpenses : Model -> Html Msg
viewExpenses model =
    section [ class "section pt-0" ]
        [ model.expenses
            |> Dict.values
            |> List.map viewExpense
            |> div []
        ]


viewExpense : API.Expense -> Html Msg
viewExpense expense =
    div [ class "box mb-3" ]
        [ p [ class "title is-4" ]
            [ expense.amount
                |> String.fromInt
                |> String.reverse
                |> String.wrapWith 3 ","
                |> String.reverse
                |> text
            ]
        , p [ class "subtitle is-6" ]
            [ text (expense.date ++ " " ++ expense.description)
            ]
        ]


viewExpenseInput : Model -> Html Msg
viewExpenseInput model =
    section [ class "section py-3" ]
        [ div [ class "box" ]
            [ div [ class "field has-addons" ]
                [ p [ class "control" ]
                    [ input
                        [ class "input"
                        , type_ "number"
                        , value model.inputDay
                        , placeholder "日"
                        , onInput (Input ExpenseDate)
                        ]
                        []
                    ]
                , p [ class "control" ]
                    [ input
                        [ class "input"
                        , value model.inputAmount
                        , type_ "number"
                        , placeholder "円"
                        , onInput (Input ExpenseAmount)
                        ]
                        []
                    ]
                , p [ class "control is-expanded" ]
                    [ input
                        [ class "input"
                        , value model.inputDescription
                        , type_ "text"
                        , placeholder "メモ"
                        , onInput (Input ExpenseDescription)
                        ]
                        []
                    ]
                , p [ class "control" ]
                    [ span [ class "button is-primary", onClick SubmitExpense ] [ text "追加" ] ]
                ]
            ]
        ]
