namespace TodoServer

open System.Collections.Generic
open System.Linq

type TaskId = int64

type Task = {
  id : TaskId
  text : string
  finished : bool
}

module Tasks =
  let mutable private nextId = int64 1

  let private tasks =
    Dictionary<TaskId, Task>()

  let getTasks() = 
    tasks.Values.ToArray ()


  let getTask (taskId : TaskId) =
    match tasks.TryGetValue taskId with
    | (false, _)   -> None
    | (true, task) -> Some task    


  let newTask text =
    let task = { id = nextId; text = text; finished = false }
    nextId <- nextId + int64 1
    tasks.Add( task.id, task )
    task


  let updateTask (task : Task) =
    match tasks.TryGetValue task.id with
    | (false, _) -> None
    | (true, found) ->
      let updated = { found with text = task.text; finished = task.finished }
      tasks.[found.id] <- updated
      Some updated


  let deleteTask (taskId : TaskId) =
    tasks.Remove taskId
