module Main exposing (main)

import Html exposing (..)
import Html.Events as Ev
import Html.Attributes as Attr


type alias Model =
    { input : String
    , tasks : List String
    }


view model =
    div []
        [ form []
            [ input
                [ Attr.value model.input
                ]
                []
            , button [] [ text "OK" ]
            ]
        , viewTasks model.tasks
        ]


viewTasks tasks =
    ul []
        (List.map (\task -> li [] [ text task ]) tasks)


update msg model =
    model


main =
    beginnerProgram
        { model =
            Model "" [ "TEA vorstellen" ]
        , view = view
        , update = update
        }
