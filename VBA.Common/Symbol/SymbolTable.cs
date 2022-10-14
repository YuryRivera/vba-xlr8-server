namespace VBA.Common.Symbol;

public class SymbolTable
{

    public RootScope? Root { get; set; }
    public SymbolTable? UpperScope { get; set; }
    public List<SymbolTable> Scope { get; set; } = new();


}