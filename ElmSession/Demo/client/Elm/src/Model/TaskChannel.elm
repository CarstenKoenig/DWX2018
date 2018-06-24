module Model.TaskChannel exposing (TaskChange(..), decoder)

import Json.Decode as Json exposing (Decoder)
import Model.Task as Task exposing (Task, TaskId)


type TaskChange
    = NewTask Task
    | UpdateTask Task
    | DeleteTask TaskId


decoder : Decoder TaskChange
decoder =
    Json.field "tag" Json.string
        |> Json.andThen
            (\tag ->
                case tag of
                    "NewTask" ->
                        Json.map NewTask (Json.field "contents" Task.decoder)

                    "UpdateTask" ->
                        Json.map UpdateTask (Json.field "contents" Task.decoder)

                    "DeleteTask" ->
                        Json.map DeleteTask (Json.field "contents" Json.int)

                    tag ->
                        Json.fail ("unknown case " ++ tag)
            )
