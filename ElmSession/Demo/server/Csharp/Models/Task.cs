namespace TodoServer.Models
{
  public class Task
  {
    public System.Int64 Id { get; set; }
    public string Text { get; set; }
    public bool Finished { get; set; }
  }
}