module Api.Task exposing (Url, get, getAll, postUpdate, postNew)

import Http exposing (Request)
import Json.Decode as Json
import Json.Encode as Enc
import Todo exposing (TaskId, Task)


type alias Url =
    String


get : Url -> TaskId -> Request (Maybe Task)
get baseUrl taskId =
    Http.get (baseUrl ++ "todos/" ++ toString taskId) (Json.maybe Todo.decoder)


getAll : Url -> Request (List Task)
getAll baseUrl =
    Http.get (baseUrl ++ "todos") (Json.list Todo.decoder)


postUpdate : Url -> Task -> Request Task
postUpdate baseUrl task =
    Http.post (baseUrl ++ "todos") (Http.jsonBody (Todo.encode task)) Todo.decoder


postNew : Url -> String -> Request Task
postNew baseUrl text =
    Http.post (baseUrl ++ "todos") (Http.jsonBody (Enc.string text)) Todo.decoder
