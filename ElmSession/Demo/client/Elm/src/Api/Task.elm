module Api.Task exposing (Url, get, getAll, postUpdate, postNew)

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


postUpdate : Url -> Task -> Request Task
postUpdate baseUrl task =
    Http.post (baseUrl ++ "todos") (Http.jsonBody (Model.Task.encode task)) Model.Task.decoder


postNew : Url -> String -> Request Task
postNew baseUrl text =
    Http.post (baseUrl ++ "todos") (Http.jsonBody (Enc.string text)) Model.Task.decoder
