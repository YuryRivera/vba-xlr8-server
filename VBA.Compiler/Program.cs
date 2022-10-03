using VBA.Compiler.Package;
using VBA.Compiler.Responses;

namespace VBA.Compiler;

public static class Program
{
    public static int Main(string[] args)
    {
        bool hasConfig = File.Exists("vba.json");
        if (!hasConfig)
        {
            NodeOutput.SendError(new AppError
            {
                Message = $"No vba.json file found in current folder",
            });
            return 0;
        }

        var packager = new StagePack();
        packager.CopyXmlFiles();
        const string path = "sample_data.xlsx";
        // var fileInfo = new FileInfo(path);
        // var package = new ExcelPackage();
        // ExcelWorksheet? sheet = package.Workbook.Worksheets.Add("test");
        // sheet.SetValue("A1", 69);
        // Thread.Sleep(1000);
        //
        // StreamReader stream = File.OpenText("Objects/Aurora.vba");
        // string vbaSource = stream.ReadToEnd();
        //
        // package.Workbook.CreateVBAProject();
        // var project = 
        return 0;
    }
}