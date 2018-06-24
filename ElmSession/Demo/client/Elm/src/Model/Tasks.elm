module Model.Tasks
    exposing
        ( Tasks
        , Filter(..)
        , empty
        , fromList
        , insert
        , delete
        , get
        , getSortedTaskList
        , decoder
        )

import Dict exposing (Dict)
import Model.Task exposing (TaskId, Task)
import Json.Decode as Json exposing (Decoder)


type alias Tasks =
    Dict TaskId Task


type alias TasksRecord rest =
    { rest | tasks : Tasks }


type Filter
    = All
    | Pending
    | Completed


empty : Tasks
empty =
    Dict.empty


fromList : List Task -> Tasks
fromList =
    List.map (\task -> ( task.id, task )) >> Dict.fromList


insert : Task -> Tasks -> Tasks
insert task =
    Dict.insert task.id task


delete : TaskId -> Tasks -> Tasks
delete taskId =
    Dict.remove taskId


get : TaskId -> TasksRecord rest -> Maybe Task
get taskId model =
    Dict.get taskId model.tasks


getSortedTaskList : Filter -> TasksRecord rest -> List Task
getSortedTaskList filter model =
    let
        applyFilter task =
            case filter of
                All ->
                    True

                Pending ->
                    not task.finished

                Completed ->
                    task.finished
    in
        model.tasks
            |> Dict.values
            |> List.filter applyFilter
            |> sortList


sortList : List Task -> List Task
sortList tasks =
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


decoder : Decoder Tasks
decoder =
    Json.map fromList (Json.list Model.Task.decoder)
