module Main exposing (..)

import Api.Task exposing (Url)
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
import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Model.Task exposing (TaskId, Task)


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
    , inputText : String
    , activeTask : Maybe ( TaskId, Maybe String )
    , tasks : Dict TaskId Task
    , httpError : Maybe Http.Error
    , isBusy : Bool
    }


submitAllowed : Model -> Bool
submitAllowed model =
    String.length model.inputText > 5


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
    | MouseOver TaskId
    | MouseOut TaskId
    | SubmitNewTask String
    | NewTaskResponse (Result Http.Error Task)
    | ChangeTaskText String
    | SubmitTaskTextChange TaskId String


init : Flags -> ( Model, Cmd Msg )
init flags =
    { flags = flags
    , inputText = ""
    , activeTask = Nothing
    , tasks = Dict.empty
    , httpError = Nothing
    , isBusy = True
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
            }
                ! [ Http.send NewTaskResponse (Api.Task.postNew model.flags.baseUrl text) ]

        NewTaskResponse (Err err) ->
            { model
                | httpError = Just err
                , isBusy = False
            }
                ! []

        NewTaskResponse (Ok task) ->
            let
                newTasks =
                    model.tasks
                        |> Dict.insert task.id task
            in
                { model
                    | tasks = newTasks
                    , httpError = Nothing
                    , isBusy = False
                }
                    ! []

        MouseOver tId ->
            case model.activeTask of
                Just ( _, Just _ ) ->
                    model ! []

                _ ->
                    { model | activeTask = Just ( tId, Nothing ) } ! []

        MouseOut tId ->
            case model.activeTask of
                Just ( activeId, Nothing ) ->
                    if activeId == tId then
                        { model | activeTask = Nothing } ! []
                    else
                        model ! []

                _ ->
                    model ! []

        ChangeTaskText text ->
            case model.activeTask of
                Just ( tId, _ ) ->
                    { model | activeTask = Just ( tId, Just text ) } ! []

                Nothing ->
                    model ! []

        SubmitTaskTextChange taskId text ->
            let
                neueTasks =
                    changeText taskId text model.tasks

                neuActive =
                    Just ( taskId, Nothing )
            in
                { model | tasks = neueTasks, activeTask = neuActive } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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
                                , Attr.disabled model.isBusy
                                ]
                                [ Grid.row
                                    [ Row.attrs [ Size.w100 ] ]
                                    [ Grid.col [ Col.xs ]
                                        [ Input.text
                                            [ Input.onInput ChangeInputText
                                            , Input.placeholder "was ist zu tun?"
                                            , Input.attrs [ Size.w100 ]
                                            , Input.value model.inputText
                                            ]
                                        ]
                                    , Grid.col [ Col.xsAuto ]
                                        [ Button.button
                                            [ Button.primary
                                            , Button.disabled (not <| submitAllowed model)
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
        ( isActive, activeText ) =
            case model.activeTask of
                Just ( activeId, activeText ) ->
                    ( activeId == task.id, activeText )

                Nothing ->
                    ( False, Nothing )

        textStyle =
            if task.finished then
                Attr.style [ ( "text-decoration", "line-through" ) ]
            else
                Attr.style []

        optColor =
            if task.finished then
                [ List.light ]
            else
                [ List.warning ]

        optActive =
            if isActive then
                [ List.active ]
            else
                []

        events =
            [ List.attrs
                [ Ev.onMouseOver (MouseOver task.id)
                , Ev.onMouseOut (MouseOut task.id)
                ]
            ]

        content =
            if isActive then
                let
                    text =
                        activeText |> Maybe.withDefault task.text
                in
                    Form.formInline
                        [ Ev.onSubmit (SubmitTaskTextChange task.id text) ]
                        [ Input.text
                            [ Input.onInput ChangeTaskText
                            , Input.attrs [ Size.w75 ]
                            , Input.value text
                            ]
                        ]
            else
                Html.strong
                    [ textStyle ]
                    [ Html.text task.text
                    ]
    in
        List.li
            (optActive ++ optColor ++ events)
            [ Grid.row
                []
                [ Grid.col
                    [ Col.xs10 ]
                    [ content ]
                ]
            ]
