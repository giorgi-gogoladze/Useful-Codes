module UploadManager

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
    | FilesSelected of File list
    | ReadErr of exn
    | SmallMsg of Uploader.Msg
    | LargeMsg of UploadLarge.Msg
    |Start
    |Close
    |RefreshHome

[<AutoOpen>]
module SomeUtils=
    
    let openFile (fileEvent: Event) dispatch =
        let files: Browser.Types.FileList = !!fileEvent.target?files
        if files.length>0 then
            [for i in 0..files.length-1->files.[i]] |> FilesSelected |> dispatch

    let chunkSize = 8 * 1024 * 1024

type Model =
    { SmallModel: Uploader.Model option
      LargeModel: UploadLarge.Model option
      Files:File list
      Group:string
      }

let init gr =
    { SmallModel=None
      LargeModel=None
      Files=[]
      Group=gr
    },
    Cmd.none

let update msg (model: Model) : Model * Cmd<Msg> =
    match msg with
    |Close|RefreshHome->model,[]
    | FilesSelected f ->
        {model with Files=f},Cmd.ofMsg Start
    | Start ->
        match model.Files with
        |[]->{model with Files=[]; SmallModel=None;LargeModel=None},Cmd.ofMsg RefreshHome
        |f::_->
        if f.size <= chunkSize then
            let a,b=Uploader.init f model.Group
            { model with  SmallModel=Some a},
            Cmd.map SmallMsg b
        else
            let a,b=UploadLarge.init f model.Group
            {model with LargeModel=Some a},Cmd.map LargeMsg b
    |SmallMsg Uploader.Finished->
        match model.Files with
        |_::t->{model with Files=t},Cmd.ofMsg Start
        //for completensess
        |_->model,[]
            //{model with SmallModel=None},[]
    |SmallMsg m->
        let a,b=Uploader.update m model.SmallModel.Value
        {model with SmallModel=Some a},Cmd.map SmallMsg b
    |LargeMsg UploadLarge.Finished->
        match model.Files with
        |_::t->{model with Files=t},Cmd.ofMsg Start
        //for completensess
        |_->model,[]
    |LargeMsg m->
        let a,b=UploadLarge.update m model.LargeModel.Value
        {model with LargeModel=Some a},Cmd.map LargeMsg b
    | ReadErr e -> model, []



let vErr disp (a:string)=
    div[][
        p[][str <|"Error: "+a]
        button [Class"button is-warning";OnClick(fun _->Close|>disp)] [str"Close"]
        ]
let vSuccessLarge (a:string) _= str <|"Please wait. "+a
let vLoading (a:string)= str <|"Please wait... Chunks remaining: "+a
let vLoading1 = str "Loading"
let vNoop= str ""
let v0 _=str""

let viewUpload (model: Model) dispatch=
    let v =
        div [ Class "file" ] [
            label [ Class "file-label" ] [
                input [ Class "file-input"
                        Type "file"
                        HTMLAttr.Multiple true
                        OnChange(fun a -> openFile a dispatch)
                        Name "resume" ]
                span [ Class "file-cta" ] [
                    span [ Class "file-icon" ] [
                        i [ Class "fas fa-upload" ] []
                    ]
                    span [ Class "file-label" ] [
                        str "Choose a file…"
                    ]
                ]
            ]
        ]
    div[]
        [
        //div [ Class "field" ] [
        //    label [ Class "label" ] [str "Description"]
        //    input [ Class <| "input "
        //            Type "text"
        //            Value(model.Descr)
        //            OnChange(fun e -> e.target?value |> ChangeDescr |> dispatch) ]]
        p[Class"title"][str"Upload file"]
        v
        hr[]
        //button [ Class "button is-warning";OnClick(fun _ -> CloseAndRefresh |> dispatch) ] [str "Close"]
        ]


let view (model: Model) dispatch =   
    let cont=
        match model.SmallModel,model.LargeModel with
        |Some v,_->
            Uploader.view v (SmallMsg>>dispatch)
        |_,Some v->
            UploadLarge.view v (LargeMsg>>dispatch)
        |_->viewUpload model dispatch
    div[][
        cont
    ]
