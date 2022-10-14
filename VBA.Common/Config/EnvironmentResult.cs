using System.ComponentModel.DataAnnotations;

namespace VBA.Common.Config;

public class EnvironmentResult
{
    public EnvironmentResult(IEnumerable<EnvironmentVar> variables, IEnumerable<ValidationException> errors)
    {
        Variables = variables;
        Errors = errors;
    }

    public IEnumerable<EnvironmentVar> Variables { get; }
    public IEnumerable<ValidationException> Errors { get; }
}