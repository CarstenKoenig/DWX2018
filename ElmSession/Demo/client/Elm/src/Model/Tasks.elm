module Model.Tasks
    exposing
        ( Tasks
        , empty
        , fromList
        , insert
        , get
        , getSortedTaskList
        )

import Dict exposing (Dict)
import Model.Task exposing (TaskId, Task)


type alias Tasks =
    Dict TaskId Task


type alias TasksRecord rest =
    { rest | tasks : Tasks }


empty : Tasks
empty =
    Dict.empty


fromList : List Task -> Tasks
fromList =
    List.map (\task -> ( task.id, task )) >> Dict.fromList


insert : Task -> Tasks -> Tasks
insert task =
    Dict.insert task.id task


get : TaskId -> TasksRecord rest -> Maybe Task
get taskId model =
    Dict.get taskId model.tasks


getSortedTaskList : TasksRecord rest -> List Task
getSortedTaskList model =
    model.tasks
        |> Dict.values
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
