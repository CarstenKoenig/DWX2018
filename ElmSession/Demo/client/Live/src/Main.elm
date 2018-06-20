module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as List
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Space
import Dict exposing (Dict)
import FontAwesome as FontA
import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias TaskId =
    Int


type alias Task =
    { id : TaskId
    , text : String
    , finished : Bool
    }


type alias Tasks =
    Dict TaskId Task


type alias Model =
    { flags : Flags
    , isBusy : Bool
    , httpError : Maybe Http.Error
    , tasks : Tasks
    , inputText : String
    }


type alias Flags =
    { baseUrl : String }


type Msg
    = NoOp
    | GetTasksResponse (Result Http.Error Tasks)
    | UpdateTaskResponse (Result Http.Error Task)
    | NewTaskMsg NewTaskMsg
    | TaskItemMsg TaskItemMsg


type NewTaskMsg
    = ChangeNewText String
    | SubmitNew String


type TaskItemMsg
    = ToggleTask TaskId Bool


init : Flags -> ( Model, Cmd Msg )
init flags =
    { flags = flags
    , httpError = Nothing
    , isBusy = False
    , tasks = Dict.empty
    , inputText = ""
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        GetTasksResponse (Err err) ->
            { model
                | httpError = Just err
                , isBusy = False
            }
                ! []

        GetTasksResponse (Ok newTasks) ->
            { model
                | tasks = newTasks
                , httpError = Nothing
                , isBusy = False
            }
                ! []

        UpdateTaskResponse (Err err) ->
            { model
                | httpError = Just err
                , isBusy = False
                , inputText = ""
            }
                ! []

        UpdateTaskResponse (Ok task) ->
            let
                newTasks =
                    Dict.insert task.id task model.tasks
            in
                { model
                    | tasks = newTasks
                    , httpError = Nothing
                    , isBusy = False
                    , inputText = ""
                }
                    ! []

        NewTaskMsg newMsg ->
            updateNew newMsg model

        TaskItemMsg itemMsg ->
            updateItem itemMsg model


updateNew : NewTaskMsg -> Model -> ( Model, Cmd Msg )
updateNew msg model =
    case msg of
        ChangeNewText text ->
            { model | inputText = text } ! []

        SubmitNew text ->
            { model
                | isBusy = True
                , httpError = Nothing
            }
                ! []


updateItem : TaskItemMsg -> Model -> ( Model, Cmd Msg )
updateItem msg model =
    case msg of
        ToggleTask taskId finished ->
            { model
                | isBusy = True
                , httpError = Nothing
            }
                ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Grid.container [ Attr.class "container h-100" ]
        [ Grid.row
            [ Row.attrs [ Attr.class "h-100 justify-content-center align-items-center" ] ]
            [ Grid.col
                [ Col.md12, Col.middleMd ]
                [ Card.config [ Card.outlineLight ]
                    |> Card.headerH1 [] [ Html.text "Elm-Todo-Liste" ]
                    |> Card.block [] [ Block.custom (viewNewTaskForm model |> Html.map NewTaskMsg) ]
                    |> Card.listGroup (List.map (viewTask model) (Dict.values model.tasks))
                    |> Card.view
                ]
            ]
        ]


viewNewTaskForm : Model -> Html NewTaskMsg
viewNewTaskForm model =
    let
        disabled =
            String.length model.inputText < 5 || model.isBusy
    in
        Form.formInline
            [ Ev.onSubmit (SubmitNew model.inputText)
            , Space.m0
            , Attr.disabled disabled
            , Attr.autocomplete False
            ]
            [ Grid.row
                [ Row.attrs [ Size.w100 ] ]
                [ Grid.col [ Col.xs ]
                    [ Input.text
                        [ Input.onInput ChangeNewText
                        , Input.placeholder "was ist zu tun?"
                        , Input.attrs [ Attr.id "inputText", Size.w100 ]
                        , Input.value model.inputText
                        , Input.disabled model.isBusy
                        ]
                    ]
                , Grid.col [ Col.xsAuto ]
                    [ Button.button
                        [ Button.primary
                        , Button.disabled disabled
                        , Button.attrs [ Attr.type_ "submit" ]
                        ]
                        [ Html.text "ok"
                        ]
                    ]
                ]
            ]


viewTask : Model -> Task -> List.Item Msg
viewTask model task =
    let
        isDisabled =
            model.isBusy

        textStyle =
            if task.finished then
                Attr.style [ ( "text-decoration", "line-through" ) ]
            else
                Attr.style []

        cursorPointer =
            Attr.style [ ( "cursor", "pointer" ) ]

        optColor =
            if isDisabled then
                [ List.disabled ]
            else if task.finished then
                [ List.light ]
            else
                []

        taskEvents =
            if isDisabled then
                []
            else
                [ Ev.onClick (ToggleTask task.id (not task.finished)) ]

        content =
            Html.strong [ textStyle ] [ Html.text task.text ]

        checkBox =
            Html.div (cursorPointer :: taskEvents)
                [ if task.finished then
                    FontA.iconWithOptions FontA.checkCircle FontA.Regular [] []
                  else
                    FontA.iconWithOptions FontA.circle FontA.Regular [] []
                ]
    in
        List.li
            optColor
            [ Grid.row
                [ Row.attrs [ Attr.class "justify-content-center align-items-center" ] ]
                [ Grid.col [ Col.xsAuto ] [ checkBox ]
                , Grid.col [ Col.xs ] [ content ]
                ]
                |> Html.map TaskItemMsg
            ]
