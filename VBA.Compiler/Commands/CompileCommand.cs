using CommandLine;

namespace VBA.Compiler.Commands;

[Verb("compile", true, HelpText = CommandHelp)]
public class CompileCommand
{
    #region Messages

    private const string CommandHelp = "Packs VBA code and XML files into an excel workbook macro enabled";
    private const string ProdHelp = "packs the Excel file with secured VBA module, compressed files and minify";

    #endregion

    [Option(nameof(Production), HelpText = ProdHelp)]
    public bool Production { get; set; }
}