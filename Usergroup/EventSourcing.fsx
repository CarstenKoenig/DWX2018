
open System
open System.Collections.Generic

module EventStore =

  /// Id der Aggregate
  type Id = Guid


  /// einen Stream können Events eines bestimmten Typs hinzugefügt werden
  /// und über die enthaltenen Events kann gefaltet werden
  type IEventStream<'ev> =
    abstract AddEvent : event:'ev -> unit
    abstract ProjectValue : fold:('state -> 'ev -> 'state) -> init:'state -> 'state


  type private Stream<'ev> (events : List<obj>) =
    interface IEventStream<'ev> with
      member __.AddEvent ev = events.Add (box ev)

      member __.ProjectValue fold init =
        events
        |> Seq.cast
        |> Seq.fold fold init


  /// ein Eventstore verwaltet die Streams zu den einzelnen Aggregaten
  type Store() =
    let _streams = Collections.Generic.Dictionary<Id, List<obj>>()
    let getStream id : Stream<'ev> = 
      match _streams.TryGetValue id with
      | (false, _) ->
        let list = List<obj>()
        _streams.Add(id, list)
        Stream<'ev> list
      | (true, list) ->
        Stream<'ev> list

    member __.GetStream<'ev> id =
      getStream id :> IEventStream<'ev>


// Example
let store = EventStore.Store ()

type Transaktion = Added of int | Reseted

let events = [ Added 4; Added 5; Reseted; Added 42 ]

let testId = Guid.NewGuid ()
let stream = store.GetStream testId
events |> List.iter stream.AddEvent

let result = 
  let add acc = function
    | Added n -> acc + n
    | Reseted -> 0
  stream.ProjectValue add 0

printfn "Result: %d" result

let badRequest = store.GetStream<int> testId