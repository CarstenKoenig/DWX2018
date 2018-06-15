module Model.Task exposing (TaskId, Task, encode, decoder)

import Json.Decode as Json exposing (Decoder)
import Json.Encode as Enc exposing (Value)


type alias TaskId =
    Int


type alias Task =
    { id : TaskId
    , text : String
    , finished : Bool
    }


decoder : Decoder Task
decoder =
    Json.map3 Task
        (Json.field "id" Json.int)
        (Json.field "text" Json.string)
        (Json.field "finished" Json.bool)


encode : Task -> Value
encode task =
    Enc.object
        [ ( "id", Enc.int task.id )
        , ( "text", Enc.string task.text )
        , ( "finished", Enc.bool task.finished )
        ]
