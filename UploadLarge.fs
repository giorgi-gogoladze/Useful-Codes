module UploadLarge

open System
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Thoth.Json

open Fable.React
open Fable.React.Props
open Utils
open Models
open Browser.Types
open LoadA

type Msg =
    | ChangeDescr of string
    | Init
    | Upload
    | Finish
    | InitMsg of Loader.Msg<unit>
    | UploadMsg of Loader.Msg<unit>
    | FinishMsg of Loader.Msg<unit>
    | ReadErr of exn

    //Large
    | ReadChunk
    | ReadResult of string

    |Finished

[<AutoOpen>]
module SomeUtils=
    type UploadData =
        { Name: string
          File: string
          Size: int
          FileGroup:string }

    let emptyData =
        { Name = ""
          File = ""
          Size = 0
          FileGroup="" }

    type FileInfo =
        { Line: (int * int) list
          LineLength: int
          Size:int
          SelectedFile: File option }

    let emptyInfo =
        { Line = []
          LineLength = 0
          Size=0
          SelectedFile = None }

    let upload part (f: UploadData) =
        async {
            let! a = post (toContent f) (baseUrl + "files/"+part) |>runEmpty 
            match a with        
            |_->return a
        }

    let readChunk (f: File) (offSet: int) (len: int) dispatch =
        let reader = Browser.Dom.FileReader.Create()
        let f1 = f.slice (offSet, len + offSet)

        reader.onload <-
            fun a ->
                let s =
                    Convert.ToBase64String(reader.result |> toArr)

                ReadResult (s) |> dispatch

        reader.onerror <- fun a -> ()
        reader.readAsArrayBuffer (f1)

    let chunkSize = 8 * 1024 * 1024

    let getLine x =
        let a = x % chunkSize
        let b = (x - a) / chunkSize
        b, a

type Model =
    { InitModel: Loader.Model<unit, UploadData>
      UploadModel: Loader.Model<unit, UploadData>
      FinishModel: Loader.Model<unit, UploadData>
      SelectedFile:File
      LastFile:string
      Descr: string
      FileGroup:string
      FileInfo: FileInfo }

let init (f:File) gr =
    let d, r = getLine f.size

    let line =
        List.replicate d chunkSize @ [ r ]
        |> List.zip [ 0 .. d ]

    let info: FileInfo =
        { emptyInfo with
                LineLength = line.Length
                Line = line
                Size=f.size
                SelectedFile = Some f }
    let a,b,c =
        Loader.initOnlyLoaded Loader.Z emptyData ()
        ,Loader.initOnlyLoaded Loader.Z emptyData ()
        ,Loader.initOnlyLoaded Loader.Z emptyData ()
    { InitModel=a
      UploadModel=b
      SelectedFile=f
      FinishModel=c
      Descr=""
      LastFile=""
      FileGroup=gr
      FileInfo=info },
    Cmd.ofMsg ReadChunk

let update msg (model: Model) : Model * Cmd<Msg> =
    match msg with
    | ReadChunk ->
        let inf = model.FileInfo
        let a, b =
            match inf.Line with
            | [] -> inf, Cmd.ofMsg Finish
            | [ a, b ] ->
                { inf with Line = [] }, Cmd.ofSub (fun dis -> (readChunk inf.SelectedFile.Value (chunkSize * a) b dis))
            | (a, b) :: _t ->
                { inf with Line = _t }, Cmd.ofSub (fun dis -> (readChunk inf.SelectedFile.Value (chunkSize * a) b dis))

        { model with FileInfo = a }, b
    | ReadResult s ->
        {model with LastFile=s},Cmd.ofMsg Upload
    | ChangeDescr s -> { model with Descr = s }, []
    |Init->
        let a, b =
            Loader.init
                (Loader.F (upload"init"))
                { 
                    FileGroup=model.FileGroup
                    Name = model.FileInfo.SelectedFile.Value.name
                    Size=model.FileInfo.Size
                    File = model.LastFile }

        { model with InitModel = a }, Cmd.map InitMsg b

    |InitMsg m->
        let cmd=
            match m with
            |Loader.Msg.LoadDataResponse(Loaded _) ->
                Cmd.ofMsg ReadChunk
            |_->[]
        let a,b=Loader.update m model.InitModel
        {model with InitModel=a},Cmd.batch[Cmd.map InitMsg b;cmd]
    | Upload ->
        let a, b =
            Loader.init
                (Loader.F (upload "write"))
                { 
                    FileGroup=model.FileGroup
                    Name = model.FileInfo.SelectedFile.Value.name
                    Size=model.FileInfo.Size
                    File = model.LastFile }

        { model with UploadModel = a }, Cmd.map UploadMsg b
    |UploadMsg m->
        let cmd=
            match m with
            |Loader.Msg.LoadDataResponse(Loaded _)->
                Cmd.ofMsg ReadChunk
            |_->[]
        let a,b=Loader.update m model.UploadModel
        {model with UploadModel=a},Cmd.batch[Cmd.map UploadMsg b;cmd]
    | Finish ->
        let a, b =
            Loader.init
                (Loader.F (upload "finish"))

                { 
                    FileGroup=model.FileGroup
                    Name = model.FileInfo.SelectedFile.Value.name
                    Size=model.FileInfo.Size
                    File = model.LastFile }

        { model with FinishModel = a }, Cmd.map FinishMsg b
    |FinishMsg m->
        let cmd=
            match m with
            |Loader.Msg.LoadDataResponse(Loaded _) ->
                Cmd.ofMsg Finished//Close form
            |_->[]
        let a,b=Loader.update m model.FinishModel
        {model with FinishModel=a},Cmd.batch[Cmd.map FinishMsg b;cmd]
    | ReadErr e -> model, []



let vErr disp (a:string)=
    div[][
        p[][str <|"Error: "+a]
        button [Class"button is-warning";OnClick(fun _->Finished|>disp)] [str"Close"]
        ]
let vSuccessLarge (a:string) _= str <|"Please wait. "+a
let vLoading (a:string)= str <|"Please wait... Chunks remaining: "+a
let vLoading1 = str "Loading"
let vNoop= str ""
let v0 _=str""

let view (model: Model) _dispatch =   
    let step=string model.FileInfo.Line.Length
    let viewStep _=p[][str step]
    //let a =Loader.view model.InitModel viewStep
    let b =Loader.view model.UploadModel viewStep
    let c =Loader.view model.FinishModel viewStep
    div[][
        b
        c
    ]
