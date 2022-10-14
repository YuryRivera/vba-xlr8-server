using VBA.Common.Extensions;

namespace VBA.Common.Test.Extensions;

public class UnitTest1
{
    [Fact]
    public void Should_Return_Empty_String()
    {
        const string test = "";
        string result = test.ToCapital();
        Assert.Equal(test, result);
    }
    
    [Fact]
    public void Should_Return_Upper_Single_Char_String()
    {
        const string test = "h";
        string result = test.ToCapital();
        Assert.Equal("H", result);
    }
    
    [Fact]
    public void Should_Return_Upper_Double_Char_String()
    {
        const string test = "he";
        string result = test.ToCapital();
        Assert.Equal("He", result);
    }
    
    [Fact]
    public void Should_Return_Upper_String()
    {
        const string test = "hello";
        string result = test.ToCapital();
        Assert.Equal("Hello", result);
    }
    
    [Fact]
    public void Should_Return_Lower_After_First()
    {
        const string test = "hELLO";
        string result = test.ToCapital();
        Assert.Equal("Hello", result);
    }
}