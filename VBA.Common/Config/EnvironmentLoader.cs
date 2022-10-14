using System.ComponentModel.DataAnnotations;
using System.Text.RegularExpressions;
using VBA.Common.Extensions;

namespace VBA.Common.Config;

public class EnvironmentLoader
{

    public const string EnvFileName = ".env";
    private readonly Regex _stringRex = new Regex("^\"(.*)\"$");
    private readonly Regex _string2Rex = new Regex("^'(.*)'$");
    private readonly string[] _lines;

    private EnvironmentLoader(string[] lines)
    {
        _lines = lines;
    }
    

    private EnvironmentVar? Deserialize(string value)
    {
        if (int.TryParse(value, out int iValue))
        {
            return new EnvironmentVar(iValue, EnvType.Int);
        }

        if (float.TryParse(value, out float fValue))
        {
            return new EnvironmentVar(fValue, EnvType.Float);
        }

        Match match = _stringRex.Match(value);
        if (match.Success)
        {
            return new EnvironmentVar(match.Groups[0].Value, EnvType.String);
        }

        return null;
    }

    public static EnvironmentLoader FromFile(string file = EnvFileName)
    {
        if (!File.Exists(file))
        {
            return new EnvironmentLoader(Array.Empty<string>());
        }
        
        string[] envLines = File.ReadAllLines(EnvFileName);
        return new EnvironmentLoader(envLines);
    }

    public static EnvironmentLoader GetLoader(string[] lines)
    {
        return new EnvironmentLoader(lines);
    }
    
    public EnvironmentResult ParseEnvironment()
    {
        List<EnvironmentVar> vars = new (_lines.Length);
        List<ValidationException> exceptions = new(_lines.Length);
        for (int i = 0; i < _lines.Length; i++)
        {
            string line = _lines[i];
            int fIndex = line.IndexOf('=');
            if (fIndex < 0)
            {
                string msg = $"Line [{i + 1}]: not in format 'KEY = VALUE'";
                exceptions.Add(new ValidationException(msg)
                {
                    Source = line
                });
                continue;
            }

            string key = line[..fIndex];
            string value = line[(fIndex + 1)..];
            key = key.Replace("'", "").Replace("\"", "").Trim().ToCapital();
            value = value.Trim();
            EnvironmentVar? variable = Deserialize(value);

            if (variable is null)
            {
                string msg = $"Line [{i + 1}]: '{value}' cannot be parse as int | double | boolean | sting";
                exceptions.Add(new ValidationException(msg)
                {
                    Source = line
                });
                continue;
            }

            variable.Name = key;
            vars.Add(variable);
        }

        return new EnvironmentResult(vars, exceptions);
    }
}