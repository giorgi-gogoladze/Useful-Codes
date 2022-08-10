module Uploader

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
    //| Init
    | Upload
    | Finish
    | Finished
    //| InitMsg of Loader.Msg<unit>
    | UploadMsg of Loader.Msg<unit>
    | FinishMsg of Loader.Msg<unit>
    | FileSelected of File
    | ReadErr of exn

    //Large
    | ReadChunk
    | ReadSmall
    | ReadResult of string
    | SmallResult of string

    |Close
    |CloseAndRefresh

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
          Size:int }

    let emptyInfo =
        { Line = []
          LineLength = 0
          Size=0 }

    let upload part (f: UploadData) =
        async {
            let! a = post (toContent f) (baseUrl + "files/"+part) |>runEmpty 
            match a with        
            |_->return a
        }
    let openRead (f: File) dispatch =
        let reader = Browser.Dom.FileReader.Create()

        reader.onload <-
            fun a ->
                let s =
                    Convert.ToBase64String(reader.result |> toArr) in SmallResult(s) |> dispatch

        reader.onerror <- fun a -> ()
        reader.readAsArrayBuffer (f)

type Model =
    { //InitModel: Loader.Model<unit, UploadData>
      UploadModel: Loader.Model<unit, UploadData>
      FinishModel: Loader.Model<unit, UploadData>
      LastFile:string
      SelectedFile:File
      Descr: string
      FileGroup:string
      FileInfo: FileInfo }

let init (f:File) gr =
    let b,c =
       // Loader.initOnlyLoaded Loader.Z emptyData ()
         Loader.initOnlyLoaded Loader.Z emptyData ()
        ,Loader.initOnlyLoaded Loader.Z emptyData ()
    { //InitModel=a
      UploadModel=b
      FinishModel=c
      SelectedFile=f
      Descr=""
      LastFile=""
      FileGroup=gr
      FileInfo={emptyInfo with Size=f.size} },
    Cmd.ofMsg ReadSmall

let update msg (model: Model) : Model * Cmd<Msg> =
    match msg with
    //| ReadResult s ->
    //    let h, t =
    //        model.FileInfo.Line.Length = model.FileInfo.LineLength-1, model.FileInfo.Line.Length = 0

    //    let a, b =
    //        Loader.init
    //            uploadChunk
    //            { IsHead = h
    //              IsTail = t
    //              AuthHeader=model.AuthenticateResponse.Token
    //              GroupId=0
    //              Size=model.FileInfo.Size
    //              FileGroup=model.FileGroup
    //              UserId=model.AuthenticateResponse.Id
    //              Name = model.FileInfo.SelectedFile.Value.name
    //              File = s }

    //    { model with UploadModel = a }, Cmd.map UploadMsg b
    | ChangeDescr s -> { model with Descr = s }, []
    //| UploadMsg m ->
    //    let a, b = Loader.update m model.UploadModel

    //    match m, model.FileInfo.Line with
    //    | Loader.Msg.LoadDataResponse (Loaded r), [] ->
    //        { model with
    //              FileInfo =
    //                  { model.FileInfo with
    //                        SelectedFile = None } },
    //        []
    //    | Loader.Msg.LoadDataResponse (Loaded r), _ -> model, Cmd.ofMsg ReadChunk
    //    | _ -> { model with UploadModel = a }, Cmd.map UploadMsg b

    //| UploadSmallMsg m ->
    //    let a, b = Loader.update m model.UploadSmallModel

        //match m with
        //| Loader.Msg.LoadDataResponse (Loaded r) ->
        //    { model with
        //          FileInfo =
        //              { model.FileInfo with
        //                    SelectedFile = None } },
        //    Cmd.ofMsg ReadChunk
        //| _ -> { model with UploadSmallModel = a }, Cmd.map UploadSmallMsg b
    | ReadSmall -> model, Cmd.ofSub (fun dis -> openRead model.SelectedFile dis)
    | SmallResult (s) ->
        {model with LastFile=s},Cmd.ofMsg Upload
    | Upload ->
        let a, b =
            Loader.init
                (Loader.F (upload "write"))

                { 
                    FileGroup=model.FileGroup
                    Name = model.SelectedFile.name
                    Size=model.FileInfo.Size
                    File = model.LastFile }

        { model with UploadModel = a }, Cmd.map UploadMsg b
    |UploadMsg m->
        let cmd=
            match m with
            |Loader.Msg.LoadDataResponse(Loaded _)->
                Cmd.ofMsg Finish
            |_->[]
        let a,b=Loader.update m model.UploadModel
        {model with UploadModel=a},Cmd.batch[Cmd.map UploadMsg b;cmd]
    | Finish ->
        let a, b =
            Loader.init
                (Loader.F (upload "finish"))

                { 
                    FileGroup=model.FileGroup
                    Name = model.SelectedFile.name
                    Size=model.FileInfo.Size
                    File = "" }

        { model with FinishModel = a }, Cmd.map FinishMsg b
    |FinishMsg m->
        let cmd=
            match m with
            |Loader.Msg.LoadDataResponse(Loaded _)->
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
        //a
        b
        c
    ]
