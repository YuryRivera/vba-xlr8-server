namespace VBA.Common.Typing;

public abstract class NativeType: BaseType
{
    protected NativeType()
    {
    }
}

public sealed class BooleanType: NativeType
{
    private BooleanType()
    {
        Name = "Boolean";
        IsValue = true;
    }

    public static BooleanType GetInstance() => new ();
}

public sealed class ByteType: NativeType
{
    private ByteType()
    {
        Name = "Byte";
        IsValue = true;
    }

    public static ByteType GetInstance() => new ();
}

public sealed class CurrencyType: NativeType
{
    private CurrencyType()
    {
        Name = "Currency";
        IsValue = true;
    }

    public static CurrencyType GetInstance() => new ();
}

public sealed class DateType: NativeType
{
    private DateType()
    {
        Name = "Date";
        IsValue = true;
    }

    public static DateType GetInstance() => new ();
}

public sealed class StringType: NativeType
{
    private StringType()
    {
        Name = "String";
        IsValue = true;
    }

    public static StringType GetInstance() => new ();
}