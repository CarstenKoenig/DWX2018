module Api.Task exposing (Url, get, getAll, update, delete, new, listen)

import Http exposing (Request)
import Json.Decode as Json
import Json.Encode as Enc
import Model.Task exposing (TaskId, Task)
import Model.TaskChannel as TC
import Model.Tasks as Tasks exposing (Tasks)
import WebSocket as WS


type alias Url =
    String


get : Url -> TaskId -> Request (Maybe Task)
get baseUrl taskId =
    Http.get (baseUrl ++ "todos/" ++ toString taskId) (Json.maybe Model.Task.decoder)


getAll : Url -> Request Tasks
getAll baseUrl =
    Http.get (baseUrl ++ "todos") Tasks.decoder


update : Url -> Task -> Request Task
update baseUrl task =
    Http.request
        { method = "put"
        , headers = []
        , url = baseUrl ++ "todos"
        , body = Http.jsonBody (Model.Task.encode task)
        , expect = Http.expectJson Model.Task.decoder
        , timeout = Nothing
        , withCredentials = False
        }


delete : Url -> TaskId -> Request Tasks
delete baseUrl taskId =
    Http.request
        { method = "delete"
        , headers = []
        , url = baseUrl ++ "todos/" ++ toString taskId
        , body = Http.emptyBody
        , expect = Http.expectJson Tasks.decoder
        , timeout = Nothing
        , withCredentials = False
        }


new : Url -> String -> Request Task
new baseUrl text =
    Http.post (baseUrl ++ "todos") (Http.jsonBody (Enc.string text)) Model.Task.decoder


listen : Url -> (String -> msg) -> (TC.TaskChange -> msg) -> Sub msg
listen baseUrl mapErr mapMsg =
    let
        toWs url =
            if String.startsWith "http://" url then
                "ws://" ++ String.dropLeft (String.length "http://") url
            else if String.startsWith "https://" url then
                "wss://" ++ String.dropLeft (String.length "https://") url
            else
                "ws://localhost:8080" ++ url

        decodeMsg text =
            case Json.decodeString TC.decoder text of
                Err err ->
                    mapErr err

                Ok msg ->
                    mapMsg msg

        url =
            toWs baseUrl ++ "todos/listen"
    in
        WS.listen url decodeMsg
