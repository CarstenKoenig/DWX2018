namespace TodoServer

open System
open Microsoft.AspNetCore.Hosting
open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Giraffe.GiraffeViewEngine


module Program =

    let page =
        html [] [
            head [] [
                title [] [ rawText "Todo in Elm" ]
                link [ _rel "stylesheet"; _type "text/css"; _href "lib/bootstrap/dist/css/bootstrap.min.css" ]
                link [ _rel "stylesheet"; _type "text/css"; _href "lib/fontawesome/dist/fontawesome-all.min.css" ]
                link [ _rel "stylesheet"; _type "text/css"; _href "style.css" ]
            ]
            body [] [
                script [ _src "lib/jquery/dist/jquery.js" ] []
                script [ _src "lib/popper/dist/popper.js" ] []
                script [ _src "todo.js" ] []
                script [] [
                    rawText "var app = Elm.Main.fullscreen({baseUrl: '/'});"
                ] 
            ]
        ]
  
    let getTasks() : HttpHandler =
        Tasks.getTasks()
        |> Successful.OK


    let getTask (taskId : TaskId) : HttpHandler =
        match Tasks.getTask taskId with
        | None      -> Successful.OK null
        | Some task -> Successful.OK task    


    let newTask text : HttpHandler =
        let task = Tasks.newTask text
        redirectTo false ("/todos/" + string task.id)


    let updateTask (task : Task) : HttpHandler =
        match Tasks.updateTask task with
        | None         -> RequestErrors.NOT_FOUND (sprintf "kein Task mit id %d gefunden" task.id)
        | Some updated -> Successful.OK updated


    let deleteTask (taskId : TaskId) : HttpHandler =
        if Tasks.deleteTask taskId then
            getTasks()
        else
            RequestErrors.NOT_FOUND (sprintf "kein Task mit id %d gefunden" taskId)


    let webApp =
        choose [
            GET >=> routef "/todos/%d" getTask
            GET >=> route "/todos" >=> warbler (fun _ -> getTasks ())
            POST >=> route "/todos" >=> bindJson newTask 
            PUT >=> route "/todos" >=> bindJson updateTask
            DELETE >=> routef "/todos/%d" deleteTask
            route "/ping"   >=> text "pong"
            route "/"       >=> htmlView page ]


    let configureApp (app : IApplicationBuilder) =
        app
            .UseStaticFiles()
            .UseGiraffe webApp

    let configureServices (services : IServiceCollection) =
        services.AddGiraffe() |> ignore


    [<EntryPoint>]
    let main _ =
        WebHostBuilder()
            .UseKestrel()
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(configureServices)
            .UseWebRoot("../../../../../static")
            .Build()
            .Run()
        0
