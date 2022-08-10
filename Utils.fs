module Utils

open System
open Fable.SimpleHttp
open Thoth.Json
open LoadA

#if DEBUG
[<Literal>]
let baseUrl = "http://localhost:5286/"
#else
[<Literal>]
let baseUrl = "/"
#endif

let logInfo s = Browser.Dom.console.log (s)

let get url=
    Http.request url
    |> Http.method GET
    |> Http.header (Headers.contentType "application/json")

let post cont url=
    Http.request url
    |> Http.method POST
    |> Http.content cont
    |> Http.header (Headers.contentType "application/json")

let withHeader h (req:HttpRequest)=
    req
    |>Http.header h

let inline toContent a=
    let ser = Encode.Auto.toString (0, a)
    BodyContent.Text ser

let inline run (req:HttpRequest) decoder=
    async{
    let! r=
        req
        |>Http.send
    match r.statusCode with
    |200|201->
        let decoded = Decode.fromString decoder r.responseText
        match decoded with
        | Ok d ->return Loaded d
        | Error e ->
            logInfo <| "Error while decoding data:" + e
            return OtherError e
    |_->return OtherError ""
    }

let inline runEmpty (req:HttpRequest)=
    async{
    let! r=
        req
        |>Http.send
    match r.statusCode with
    |200|201->
        return Loaded ()
    |_->return OtherError r.responseText
    }


let zeroLoader _=async{return Loading}

open Fable.Core.JsInterop
open Fable.Core

[<Emit("getCookie($0);")>]
let getCookie _s:string = jsNative

[<Emit("setCookie($0,$1,$2)")>]
let setCookie (_:string) (_:string) (_:int) = jsNative

[<Emit("eraseCookie($0);")>]
let eraseCookie (_:string) = jsNative


[<Emit("unescape(encodeURIComponent($0))")>]
let unescape s = jsNative

let inline getValueFromOption a =
    match a with
    | Some a -> string a
    | _ -> ""

let getValueFromOptionalDate (a: DateTime option) =
    match a with
    | Some a -> a.ToShortDateString()
    | _ -> ""

[<Emit("new Uint8Array($0)")>]
let toArr (_: obj) : byte [] = jsNative


