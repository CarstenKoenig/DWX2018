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
    , isBusy : Bool
    , httpError : Maybe Http.Error
    , tasks : Dict TaskId Task
    , inputText : String
    , activeTask : Maybe ( TaskId, String )
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
    | SubmitNewTask String
    | UpdateTaskResponse (Result Http.Error Task)
    | ToggleTask TaskId Bool


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
                ! [ Http.send UpdateTaskResponse (Api.Task.postNew model.flags.baseUrl text) ]

        UpdateTaskResponse (Err err) ->
            { model
                | httpError = Just err
                , isBusy = False
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
                            ! [ Http.send UpdateTaskResponse (Api.Task.postUpdate model.flags.baseUrl task) ]


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
                    ( False, "" )

        isDisabled =
            model.isBusy

        textStyle =
            if task.finished then
                Attr.style [ ( "text-decoration", "line-through" ) ]
            else
                Attr.style []

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

        optStyles =
            [ List.attrs [ Attr.style [ ( "cursor", "pointer" ) ] ] ]

        events =
            let
                click =
                    if isDisabled then
                        []
                    else
                        [ Ev.onClick (ToggleTask task.id (not task.finished)) ]
            in
                [ List.attrs click ]

        content =
            Html.strong
                [ textStyle ]
                [ Html.text task.text
                ]
    in
        List.li
            (optStyles ++ optColor ++ events)
            [ Grid.row
                []
                [ Grid.col
                    [ Col.xs10 ]
                    [ content ]
                ]
            ]
