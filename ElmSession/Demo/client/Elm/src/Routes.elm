module Routes exposing (Route(..), locationToRoute, routeToUrl)

import Navigation as Nav exposing (Location)
import UrlParser as Url exposing (Parser, (</>))


type alias Url =
    String


type Route
    = ShowAll
    | ShowPending
    | ShowCompleted


locationToRoute : Location -> Maybe Route
locationToRoute =
    Url.parsePath route


routeToUrl : Route -> Url
routeToUrl route =
    case route of
        ShowAll ->
            "/"

        ShowPending ->
            "/pending"

        ShowCompleted ->
            "/completed"


route : Parser (Route -> a) a
route =
    let
        basePart =
            Url.top
    in
        Url.oneOf
            [ Url.map ShowAll basePart
            , Url.map ShowPending (basePart </> Url.s "pending")
            , Url.map ShowCompleted (basePart </> Url.s "completed")
            ]
