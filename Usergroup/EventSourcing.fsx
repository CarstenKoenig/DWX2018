
open System

module EventStore =

  /// Id der Aggregate
  type Id = Guid


  /// einen Stream können Events eines bestimmten Typs hinzugefügt werden
  /// und über die enthaltenen Events kann gefaltet werden
  type IEventStream<'ev> =
    abstract AddEvent : event:'ev -> unit
    abstract ProjectValue : fold:('state -> 'ev -> 'state) -> init:'state -> 'state


  type private Stream<'ev> (evs : 'ev seq) =
    let _events = Collections.Generic.List<'ev> evs
    
    interface IEventStream<'ev> with
      member __.AddEvent ev = _events.Add ev

      member __.ProjectValue fold init =
        Seq.fold fold init _events


  /// ein Eventstore verwaltet die Streams zu den einzelnen Aggregaten
  type Store() =
    let _streams = Collections.Generic.Dictionary<Id, Type*obj>()
    let getStream id : Stream<'ev> = 
      match _streams.TryGetValue id with
      | (false, _) ->
        let stream = Stream<'ev> Seq.empty
        _streams.Add(id, (typeof<'ev>, box stream))
        stream
      | (true, (t, stream)) when t = typeof<'ev> ->
        unbox stream
      | (true, (t, _)) ->
        invalidOp (sprintf "the requested stream only accepcts events of type %s" t.Name)

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