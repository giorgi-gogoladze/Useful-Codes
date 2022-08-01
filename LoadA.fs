module LoadA

open System
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Thoth.Json

open Fable.React
open Fable.React.Props
open Models

type Loader<'a> =
    | Loading
    | Loaded of 'a
    | OtherError of string

[<RequireQualifiedAccess>]
module Loader=
    type Msg<'a> =
        | Load
        | LoadDataResponse of Loader<'a>
        | LoadErr of exn

        
    type Operator<'a,'b>=
        ///Some func
        |F of ('b -> Async<Loader<'a>>)
        ///Zero
        |Z

    type Model<'a, 'b> =
        { Result: Loader<'a>
          Error: string option
          Op: Operator<'a,'b>
          Arg: 'b }

    let map f (arg:Loader<'a>)=
        match arg with
        |Loaded a->Loaded (f a)
        |_->arg

    let get (arg:Loader<'a>)=
        match arg with
        |Loaded a->a
        |_->failwith"Can not get data from arg"//Unchecked.defaultof<'a>

    let onSuccess f def=
        function
        |LoadDataResponse (Loaded a)->f a
        |_->def

    let onSuccessWith f def=
        function
        |LoadDataResponse (Loaded a)->f a
        |_->def()

    let zeroLoader a=async{ return Loaded a}

    let init<'a, 'b> op arg : Model<'a, 'b> * Cmd<Msg<'a>> =
        { Result = Loading; Op = op; Arg = arg; Error=None }, Cmd.ofMsg Load

    let initOnly<'a, 'b> op arg : Model<'a, 'b> = { Result = Loading; Op = op; Arg = arg; Error=None }

    let initOnlyLoaded<'a, 'b> op arg (ini: 'a) : Model<'a, 'b> =
        { Result = Loaded ini
          Op = op
          Error=None
          Arg = arg }

    let update<'a, 'b> (msg: Msg<'a>) (model: Model<'a, 'b>) : Model<'a, 'b> * Cmd<Msg<'a>> =
        match msg with
        | Load ->
            { model with Result = Loading },
            match model.Op with
            |F op->
                Cmd.OfAsync.either op model.Arg LoadDataResponse LoadErr
            |Z->[]
        | LoadDataResponse (Loaded a) -> { model with Result = Loaded a }, Cmd.none
        | LoadDataResponse a -> { model with Result = a }, Cmd.none
        | LoadErr err -> { model with Result = OtherError  err.Message }, Cmd.none

    let view<'a, 'b> (model: Model<'a, 'b>) f =
        match model.Result with
        | Loaded a -> f a
        | Loading -> str "Loading..."
        | OtherError e -> str <|"Error!!!"+e

    let viewCustom<'a, 'b> (model: Model<'a, 'b>) fSucc fLoading fErrr : ReactElement =
        match model.Result with
        | Loaded a -> fSucc a
        | Loading -> fLoading
        | OtherError e -> fErrr e
