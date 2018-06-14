module Main exposing (..)

import Html as Html exposing (Html)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { flags : Flags }


type alias Flags =
    { baseUrl : String }


type Msg
    = NoOp


init : Flags -> ( Model, Cmd Msg )
init flags =
    { flags = flags } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Html.h1 [] [ Html.text "Hello Elm" ]
