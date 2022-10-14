namespace VBA.Common.Config;

public enum EnvType
{
    Int,
    String,
    Float,
    Boolean
}

public class EnvironmentVar
{
    public string Name { get; set; } = string.Empty;
    public EnvType ClrType { get; }
    public object Value { get; }

    public EnvironmentVar(object value, EnvType clrType)
    {
        Value = value;
        ClrType = clrType;
    }
}