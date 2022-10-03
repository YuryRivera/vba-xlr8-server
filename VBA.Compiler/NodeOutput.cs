using System.Text.Json;

namespace VBA.Compiler;

public static class NodeOutput
{

    public static void Send(object message)
    {
        string msg = JsonSerializer.Serialize(message);
        Console.WriteLine(msg);
    }
    
    public static void SendError(object message)
    {
        string msg = JsonSerializer.Serialize(message);
        Console.Error.WriteLine(msg);
    }
}