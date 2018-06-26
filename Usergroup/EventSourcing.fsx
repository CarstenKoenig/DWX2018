open System
open System.Collections.Generic

module EventStore =

  /// Id eines Streams
  type Id = Guid


  /// einen Stream können Events eines bestimmten Typs hinzugefügt werden
  /// und über die enthaltenen Events kann gefaltet werden
  type IEventStream<'ev> =
    abstract AddEvent : event:'ev -> unit
    abstract ProjectValue : fold:('state -> 'ev -> 'state) -> init:'state -> 'state


  type private Stream<'ev> (events : List<'ev>) =
    interface IEventStream<'ev> with
      member __.AddEvent ev = events.Add ev

      member __.ProjectValue fold init =
        events
        |> Seq.fold fold init


  /// ein Eventstore verwaltet die Streams zu den einzelnen Aggregaten
  type Store() =
    let _streams = Collections.Generic.Dictionary<Id, Type*obj>()
    let getStream id : Stream<'ev> = 
      match _streams.TryGetValue id with
      | (false, _) ->
        let list = List<'ev>()
        _streams.Add(id, (typeof<'ev>, box list))
        Stream list
      | (true, (typ, list)) when typ = typeof<'ev> ->
        Stream (unbox list)
      | (true, (typ, _)) -> 
        failwith (sprintf "requested a %s-stream but it's of type %s" 
                    typeof<'ev>.Name typ.Name)      

    member __.GetStream<'ev> id =
      getStream id :> IEventStream<'ev>


// Example
let store = EventStore.Store ()

[<Measure>] type Eur

type Transaktion = 
  | Gutschrift of decimal<Eur> 
  | Lastschrift of decimal<Eur>

let events = [ Gutschrift 100.00m<Eur>; Gutschrift 80.0m<Eur>; Lastschrift 13.40m<Eur> ]

let testId = Guid.NewGuid ()
let stream = store.GetStream testId
events |> List.iter stream.AddEvent

let kontostand = 
  let add acc = function
    | Gutschrift gut -> acc + gut
    | Lastschrift last -> acc - last
  stream.ProjectValue add 0.0m<Eur>

printfn "Result: %.2f€" kontostand

[<Measure>] type Usd
let eur2Usd (eur : decimal<Eur>) =
  1.1702m<Usd/Eur> * eur

printfn "Result: %.2f$" (eur2Usd kontostand)

let runtimeErr = 
  store.GetStream<int> testId
