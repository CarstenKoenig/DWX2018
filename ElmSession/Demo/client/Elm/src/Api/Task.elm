module Api.Task exposing (Url, get, getAll, update, delete, new)

import Http exposing (Request)
import Json.Decode as Json
import Json.Encode as Enc
import Model.Task exposing (TaskId, Task)


type alias Url =
    String


get : Url -> TaskId -> Request (Maybe Task)
get baseUrl taskId =
    Http.get (baseUrl ++ "todos/" ++ toString taskId) (Json.maybe Model.Task.decoder)


getAll : Url -> Request (List Task)
getAll baseUrl =
    Http.get (baseUrl ++ "todos") (Json.list Model.Task.decoder)


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


delete : Url -> TaskId -> Request (List Task)
delete baseUrl taskId =
    Http.request
        { method = "delete"
        , headers = []
        , url = baseUrl ++ "todos/" ++ toString taskId
        , body = Http.emptyBody
        , expect = Http.expectJson (Json.list Model.Task.decoder)
        , timeout = Nothing
        , withCredentials = False
        }


new : Url -> String -> Request Task
new baseUrl text =
    Http.post (baseUrl ++ "todos") (Http.jsonBody (Enc.string text)) Model.Task.decoder
