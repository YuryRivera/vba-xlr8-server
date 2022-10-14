namespace VBA.Common.Extensions;

public static class StringExt
{
    public static string ToCapital(this string ss)
    {
        return ss.Length switch
        {
            0 => "",
            1 => ss.ToUpper(),
            _ => char.ToUpper(ss[0]) + ss[1..].ToLower()
        };
    }
}