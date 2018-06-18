module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias Model =
    { flags : Flags }


type alias Flags =
    { baseUrl : String }


type Msg
    = NoOp


init : Flags -> ( Model, Cmd Msg )
init flags =
    Model flags ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "content" ]
        [ Html.div [ Attr.class "jumbotron" ]
            [ Html.h1 [] [ Html.text "Hallo Elm!" ] ]
        ]
