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
import Html as Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
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
    }


submitAllowed : Model -> Bool
submitAllowed model =
    String.length model.inputText > 5


sortTasks : List Task -> List Task
sortTasks tasks =
    let
        sorting t1 t2 =
            case ( t1.finished, t2.finished ) of
                ( True, True ) ->
                    compare t1.id t2.id

                ( False, False ) ->
                    compare t1.id t2.id

                ( False, True ) ->
                    LT

                ( True, False ) ->
                    GT
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
    | ChangeInputText String
    | ChangeTaskText String
    | MouseOver TaskId
    | MouseOut TaskId
    | SubmitNewTask String
    | SubmitTaskTextChange TaskId String


init : Flags -> ( Model, Cmd Msg )
init flags =
    { flags = flags
    , inputText = ""
    , activeTask = Nothing
    , tasks =
        [ ( 1, Task 1 "Test" True )
        , ( 2, Task 2 "nicht fertig" False )
        ]
            |> Dict.fromList
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        ChangeInputText text ->
            { model | inputText = text } ! []

        ChangeTaskText text ->
            case model.activeTask of
                Just ( tId, _ ) ->
                    { model | activeTask = Just ( tId, Just text ) } ! []

                Nothing ->
                    model ! []

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

        SubmitNewTask text ->
            let
                neuerTask =
                    Task
                        (Dict.size model.tasks + 1)
                        text
                        False
            in
                { model
                    | tasks = Dict.insert neuerTask.id neuerTask model.tasks
                    , inputText = ""
                }
                    ! []

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
