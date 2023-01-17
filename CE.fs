[<CLIMutable>]
type RunAppRequest=
    {
        RootDir:string
        DLLName:string
    }


let bind (f:string->Task<OpResult>) (x:Task<OpResult>)=
  task{
  let!a=x
  match a.Ok with
  |""|null->return a
  |r->return! f r
  }

type OpBuilder()=
  member __.Bind(x,f)=bind f x
  member __.Zero()=task{return{Ok=null;Error=null}}
  member __.Return a=task{return {Ok=a;Error=null}}
  member __.ReturnFrom a:Task<OpResult>=a
  member __.Combine(m, f) = bind f m
  //member __.For(sequence:seq<_>, f) =[for i in sequence->f i|>ignore]
  //member __.Delay(f: unit -> _) = f

let operate=OpBuilder()


let ofOption error = function Some s -> Ok s | None -> Error error

type ResultBuilder() =
    member __.Return(x) = Ok x

    member __.ReturnFrom(m: Result<_, _>) = m

    member __.Bind(m, f) = Result.bind f m
    member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f

    member __.Zero() = None

    member __.Combine(m, f) = Result.bind f m

    member __.Delay(f: unit -> _) = f

    member __.Run(f) = f()

    member __.TryWith(m, h) =
        try __.ReturnFrom(m)
        with e -> h e

    member __.TryFinally(m, compensation) =
        try __.ReturnFrom(m)
        finally compensation()

    member __.Using(res:#IDisposable, body) =
        __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

    member __.While(guard, f) =
        if not (guard()) then Ok () else
        do f() |> ignore
        __.While(guard, f)

    member __.For(sequence:seq<_>, body) =
        __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

let result = ResultBuilder()