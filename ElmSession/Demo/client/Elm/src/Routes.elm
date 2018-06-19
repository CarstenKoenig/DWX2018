module Routes exposing (Route(..), locationToRoute, routeToUrl)

import Navigation as Nav exposing (Location)
import UrlParser as Url exposing (Parser, (</>))
import Model.Tasks exposing (Filter(..))


type alias Url =
    String


type Route
    = Show Filter


locationToRoute : Location -> Maybe Route
locationToRoute =
    Url.parsePath route


routeToUrl : Route -> Url
routeToUrl route =
    case route of
        Show All ->
            "/"

        Show Pending ->
            "/pending"

        Show Completed ->
            "/completed"


route : Parser (Route -> a) a
route =
    let
        basePart =
            Url.top
    in
        Url.oneOf
            [ Url.map (Show All) basePart
            , Url.map (Show Pending) (basePart </> Url.s "pending")
            , Url.map (Show Completed) (basePart </> Url.s "completed")
            ]
