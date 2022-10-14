using System.Text.Json.Nodes;

namespace VBA.Common.Config;

public class VBAConfig
{
    public const string FileName = "vba.json";
    
    public string OutDIr { get; set; } = "out";

    public JsonObject[] Include { get; set; } = Array.Empty<JsonObject>();

}