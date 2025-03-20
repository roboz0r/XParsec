module XParsec.Server.Program

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Json
open Microsoft.Extensions.DependencyInjection

open System
open Oxpecker
open XParsec.Shared
open Handlers

open type Microsoft.AspNetCore.Http.TypedResults


let configureServices (services: IServiceCollection) =
    services
        .AddCors(fun options ->
            options.AddDefaultPolicy(fun policy -> policy.AllowAnyOrigin().AllowAnyMethod().AllowAnyHeader() |> ignore)
        )
        .AddRouting()
        .AddOxpecker()
    |> ignore

let configureApp (app: IApplicationBuilder) =
    app.UseRouting().UseCors().UseOxpecker(endpoints).Run(Handlers.notFound)

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    configureServices builder.Services
    let app = builder.Build()
    configureApp app
    app.Run()
    0 // Exit code
