using System.ComponentModel.DataAnnotations;
using VBA.Common.Config;
using VBA.Common.Extensions;
using Xunit.Abstractions;

namespace VBA.Common.Test.Config;

public class EnvironmentLoaderTest
{
    private readonly ITestOutputHelper _console;

    public EnvironmentLoaderTest(ITestOutputHelper console)
    {
        _console = console;
    }

    [Fact]
    public void Should_Return_Empty()
    {
        EnvironmentLoader env = EnvironmentLoader.GetLoader(Array.Empty<string>());

        EnvironmentResult result = env.ParseEnvironment();
        
        Assert.Empty(result.Variables);
        Assert.Empty(result.Errors);
    }
    
    [Fact]
    public void Should_Return_Error_Empty()
    {
        EnvironmentLoader env = EnvironmentLoader.GetLoader(new []{ "", "key ="});

        EnvironmentResult result = env.ParseEnvironment();
        
        Assert.Empty(result.Variables);
        var error = result.Errors.First();
        _console.WriteLine(error.Message);
        Assert.Equal("", error.Source);
        error = result.Errors.Skip(1).First();
        _console.WriteLine(error.Message);
        Assert.Equal("key =", error.Source);
    }
    
    [Fact]
    public void Should_Return_Error_Without_Crash()
    {
        string[] lines =
        {
            "==",
            "/*sdf",
            "'key'=''aa",
            "\"key\"=\"\"aa"
        };
        
        EnvironmentLoader env = EnvironmentLoader.GetLoader(lines);

        EnvironmentResult result = env.ParseEnvironment();
        
        Assert.Empty(result.Variables);
        Assert.Equal(lines.Length, result.Errors.Count());
        foreach ((ValidationException error, int index) in result.Errors.WithIndex())
        {
            _console.WriteLine(error.Message);
            Assert.Equal(lines[index], error.Source);

        }
    }
    
    [Fact]
    public void Should_Return_Integer()
    {
        EnvironmentLoader env = EnvironmentLoader.GetLoader(new []{ "ID = 15"});

        EnvironmentResult result = env.ParseEnvironment();

        EnvironmentVar variable = result.Variables.First();
        Assert.Equal("Id", variable.Name);
        Assert.Equal(15, variable.Value);
        Assert.Equal(EnvType.Int, variable.ClrType);
        Assert.Empty(result.Errors);
    }
    
    [Theory]
    [InlineData(.15f, "0.15")]
    [InlineData(0.17f, "0.17")]
    [InlineData(0.15f, "000.15")]
    public void Should_Return_Float(float data, string dataString)
    {
        EnvironmentLoader env = EnvironmentLoader.GetLoader(new []{ $"ID={dataString}"});

        EnvironmentResult result = env.ParseEnvironment();

        EnvironmentVar variable = result.Variables.First();
        Assert.Equal(data, variable.Value);
        Assert.Equal(EnvType.Float, variable.ClrType);
        Assert.Empty(result.Errors);
    }
}