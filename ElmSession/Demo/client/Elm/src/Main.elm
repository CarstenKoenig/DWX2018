module Main exposing (..)

import Api.Task exposing (Url)
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as BGroup
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
import Dom exposing (focus)
import FontAwesome as FontA
import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Keyboard
import Model.Task exposing (TaskId, Task)
import Task


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { flags : Flags
    , isBusy : Bool
    , httpError : Maybe Http.Error
    , tasks : Dict TaskId Task
    , inputText : String
    , editTask : Maybe ( TaskId, String )
    }


sortTasks : List Task -> List Task
sortTasks tasks =
    let
        sorting task1 task2 =
            case ( task1.finished, task2.finished ) of
                ( False, False ) ->
                    compare task1.id task2.id

                ( False, True ) ->
                    LT

                ( True, False ) ->
                    GT

                ( True, True ) ->
                    compare task1.id task2.id
    in
        List.sortWith sorting tasks


changeText : TaskId -> String -> Dict TaskId Task -> Dict TaskId Task
changeText taskId text =
    Dict.map
        (\tid task ->
            if tid == taskId then
                { task | text = text }
            else
                task
        )


type alias Flags =
    { baseUrl : String }


type Msg
    = NoOp
    | GetTasksResponse (Result Http.Error (List Task))
    | ChangeInputText String
    | SubmitNewTask String
    | UpdateTaskResponse (Result Http.Error Task)
    | ToggleTask TaskId Bool
    | EditTask TaskId String
    | ChangeEditText String
    | CancelEdit
    | DeleteTask TaskId
    | SubmitEditTask TaskId String
    | KeyDown Keyboard.KeyCode


init : Flags -> ( Model, Cmd Msg )
init flags =
    { flags = flags
    , httpError = Nothing
    , isBusy = True
    , tasks = Dict.empty
    , inputText = ""
    , editTask = Nothing
    }
        ! [ Http.send GetTasksResponse (Api.Task.getAll flags.baseUrl) ]


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

        GetTasksResponse (Ok tasks) ->
            let
                newTasks =
                    tasks
                        |> List.map (\task -> ( task.id, task ))
                        |> Dict.fromList
            in
                { model
                    | tasks = newTasks
                    , httpError = Nothing
                    , isBusy = False
                }
                    ! []

        ChangeInputText text ->
            { model | inputText = text } ! []

        SubmitNewTask text ->
            { model
                | isBusy = True
                , httpError = Nothing
                , editTask = Nothing
            }
                ! [ Http.send UpdateTaskResponse (Api.Task.new model.flags.baseUrl text)
                  , Task.attempt (always NoOp) (focus "inputText")
                  ]

        UpdateTaskResponse (Err err) ->
            { model
                | httpError = Just err
                , isBusy = False
                , editTask = Nothing
                , inputText = ""
            }
                ! []

        UpdateTaskResponse (Ok task) ->
            let
                newTasks =
                    model.tasks
                        |> Dict.insert task.id task
            in
                { model
                    | tasks = newTasks
                    , httpError = Nothing
                    , isBusy = False
                    , inputText = ""
                    , editTask = Nothing
                }
                    ! []

        ToggleTask taskId finished ->
            let
                changedTask =
                    Dict.get taskId model.tasks
                        |> Maybe.map (\task -> { task | finished = finished })
            in
                case changedTask of
                    Nothing ->
                        model ! []

                    Just task ->
                        { model
                            | isBusy = True
                            , httpError = Nothing
                        }
                            ! [ Http.send UpdateTaskResponse (Api.Task.update model.flags.baseUrl task) ]

        EditTask taskId text ->
            { model | editTask = Just ( taskId, text ) }
                ! [ Task.attempt (always NoOp) (focus ("task_" ++ toString taskId)) ]

        ChangeEditText text ->
            let
                newEdit =
                    model.editTask |> Maybe.map (\( taskId, _ ) -> ( taskId, text ))
            in
                { model | editTask = newEdit } ! []

        CancelEdit ->
            { model | editTask = Nothing }
                ! [ Task.attempt (always NoOp) (focus "inputText") ]

        SubmitEditTask taskId text ->
            let
                changedTask =
                    Dict.get taskId model.tasks
                        |> Maybe.map (\task -> { task | text = text })
            in
                case changedTask of
                    Nothing ->
                        model ! []

                    Just task ->
                        { model
                            | isBusy = True
                            , httpError = Nothing
                            , editTask = Nothing
                        }
                            ! [ Http.send UpdateTaskResponse (Api.Task.update model.flags.baseUrl task)
                              , Task.attempt (always NoOp) (focus "inputText")
                              ]

        DeleteTask taskId ->
            { model
                | editTask = Nothing
                , isBusy = True
                , httpError = Nothing
            }
                ! [ Http.send GetTasksResponse (Api.Task.delete model.flags.baseUrl taskId)
                  , Task.attempt (always NoOp) (focus "inputText")
                  ]

        KeyDown code ->
            case code of
                -- ESC
                27 ->
                    { model
                        | editTask = Nothing
                        , inputText = ""
                        , httpError = Nothing
                    }
                        ! [ Task.attempt (always NoOp) (focus "inputText") ]

                _ ->
                    model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown


view : Model -> Html Msg
view model =
    Grid.container
        []
        [ Grid.row
            [ Row.centerMd ]
            [ Grid.col
                [ Col.md12, Col.middleMd ]
                [ Card.config [ Card.outlineDark ]
                    |> Card.headerH1 [] [ Html.text "TODO:" ]
                    |> Card.block []
                        [ Block.custom
                            (Form.formInline
                                [ Ev.onSubmit (SubmitNewTask model.inputText)
                                , Space.m0
                                , Attr.disabled (model.isBusy || model.editTask /= Nothing)
                                ]
                                [ Grid.row
                                    [ Row.attrs [ Size.w100 ] ]
                                    [ Grid.col [ Col.xs ]
                                        [ Input.text
                                            [ Input.onInput ChangeInputText
                                            , Input.placeholder "was ist zu tun?"
                                            , Input.attrs [ Attr.id "inputText", Size.w100 ]
                                            , Input.value model.inputText
                                            ]
                                        ]
                                    , Grid.col [ Col.xsAuto ]
                                        [ Button.button
                                            [ Button.primary
                                            , Button.disabled (String.length model.inputText < 5)
                                            , Button.attrs [ Attr.type_ "submit" ]
                                            ]
                                            [ Html.text "ok"
                                            ]
                                        ]
                                    ]
                                ]
                            )
                        ]
                    |> Card.listGroup
                        (Dict.values model.tasks
                            |> sortTasks
                            |> List.map (viewTask model)
                        )
                    |> Card.view
                ]
            ]
        ]


viewTask : Model -> Task -> List.Item Msg
viewTask model task =
    let
        ( isEdit, editText ) =
            case model.editTask of
                Just ( activeId, activeText ) ->
                    ( activeId == task.id, activeText )

                Nothing ->
                    ( False, "" )

        isDisabled =
            model.isBusy

        textStyle =
            if task.finished then
                Attr.style
                    [ ( "text-decoration", "line-through" )
                    , ( "cursor", "pointer" )
                    ]
            else
                Attr.style [ ( "cursor", "pointer" ) ]

        optDisabled =
            if isDisabled then
                [ List.disabled ]
            else
                []

        optColor =
            if isDisabled then
                [ List.disabled ]
            else if task.finished then
                [ List.success ]
            else
                [ List.warning ]

        taskEvents =
            if isDisabled || isEdit then
                []
            else
                [ Ev.onClick (ToggleTask task.id (not task.finished)) ]

        content =
            if isEdit then
                Form.formInline
                    [ Ev.onSubmit (SubmitEditTask task.id editText)
                    , Space.m0
                    , Attr.disabled model.isBusy
                    ]
                    [ Input.text
                        [ Input.onInput ChangeEditText
                        , Input.attrs
                            [ Size.w100
                            , Ev.onBlur CancelEdit
                            , Attr.id ("task_" ++ toString task.id)
                            ]
                        , Input.value editText
                        ]
                    ]
            else
                Html.strong
                    (textStyle :: taskEvents)
                    [ Html.text task.text ]
    in
        List.li
            optColor
            [ Grid.row
                []
                [ Grid.col
                    [ Col.xs ]
                    [ content ]
                , Grid.col
                    [ Col.xsAuto ]
                    (if isEdit then
                        []
                     else
                        [ BGroup.buttonGroup
                            [ BGroup.small ]
                            [ BGroup.button
                                [ Button.outlineWarning
                                , Button.onClick (EditTask task.id task.text)
                                ]
                                [ FontA.icon FontA.edit ]
                            , BGroup.button
                                [ Button.outlineDanger
                                , Button.onClick (DeleteTask task.id)
                                ]
                                [ FontA.icon FontA.trash ]
                            ]
                        ]
                    )
                ]
            ]
