using System.Text.Json;

namespace VBA.Common.Config;

public static class ConfigLoader
{
    public static VBAConfig Load()
    {
        if (!File.Exists(VBAConfig.FileName)) return new VBAConfig();
        using FileStream stream = File.OpenRead(VBAConfig.FileName);
        var config = JsonSerializer.Deserialize<VBAConfig>(stream);
        return config ?? new VBAConfig();
    }
}