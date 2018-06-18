using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Linq;
using TodoServer.Models;

namespace TodoServer.Controllers
{
  [Produces("application/json")]
  [Route("todos")]
  public class BenutzerController : Controller
  {
    private static Int64 _nextId = 1;
    private static Dictionary<Int64, Task> _tasks = new Dictionary<Int64, Task>();

    [HttpGet]
    public IEnumerable<Task> GetTasks()
    {
      return _tasks.Values.ToArray();
    }

    [HttpGet]
    [Route("{id}")]
    public Task GetTask(Int64 id)
    {
      return _tasks.TryGetValue(id, out var task) ? task : null;
    }

    [HttpPost]
    public Task NewTask([FromBody] string text)
    {
      var id = ++_nextId;
      var task = new Task { Id = id, Text = text, Finished = false };
      _tasks.Add(id, task);
      return task;
    }

    [HttpPut]
    public Task UpdateTask([FromBody] Task task)
    {
      _tasks[task.Id] = task;
      return task;
    }

    [HttpDelete]
    [Route("{id}")]
    public IEnumerable<Task> DeleteTask(Int64 id)
    {
      _tasks.Remove(id);
      return GetTasks();
    }

  }
}