using System.Diagnostics;
using Microsoft.Extensions.Hosting;
using OfficeOpenXml;
using OfficeOpenXml.VBA;
using VBA.Compiler.Package;
using VBA.Compiler.Responses;

namespace VBA.Compiler;

public static class Program
{
    private static Process? _process;
    
    private static void Termination(object? sender,EventArgs eventArgs)
    {
        Console.WriteLine("me mataron xd :,(");
        _process?.Kill(true);
        _process?.Close();
    }

    static Program()
    {
        AppDomain.CurrentDomain.ProcessExit += Termination;
    }
    
    public static int Main(string[] args)
    {
        using IHost host = Host.CreateDefaultBuilder(args)
            .UseConsoleLifetime()
            .Build();
        
        host.Run();
        return 0;
        try
        {
            if (args[0] == "e")
            {
                var info2 = new FileInfo(args[1]);
                var package2 = new ExcelPackage(info2);
                var vba = package2.Workbook.VbaProject;
                foreach (var constant in vba.Constants)
                {
                    Console.WriteLine(constant);
                }

                foreach (var reference in vba.References)
                {
                    Console.WriteLine($"{reference.Libid}-{reference.Name}");
                }

                Directory.CreateDirectory("shit");
                Directory.CreateDirectory("unknown");
                foreach (var module in vba.Modules)
                {
                    Console.WriteLine($"{module.Name}:");
                    foreach (var attribute in module.Attributes)
                    {
                       // Console.Write($"{attribute.Name}={attribute.Value}");
                    }

                    string folder = module.Type switch
                    {
                        eModuleType.Document => "objects",
                        eModuleType.Module => "modules",
                        eModuleType.Class => "classes",
                        eModuleType.Designer => "shit",
                        _ => "unknown"
                    };
                    using var ff = File.CreateText(Path.Join(folder, $"{module.Name}.vba"));
                    ff.Write(module.Code);
                    ff.Flush();
                    ff.Close();
                }

                return 0;
            }

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

            string path = packager.Compress();
            var info = new FileInfo(path);
            var package = new ExcelPackage(info);
            ExcelWorksheet? sheet = package.Workbook.Worksheets["Aurora"];
            sheet.Cells[7, 1].Value = $"current time {DateTime.Now:hh:mm:ss}";
            InsertVba(package.Workbook);
            string newName = Path.ChangeExtension(path, "xlsm");
            var newInfo = new FileInfo(newName);
            package.SaveAs(newInfo);
            var startInfo = new ProcessStartInfo(@"C:\Program Files\Microsoft Office\Office15\EXCEL.EXE")
            {
                WindowStyle = ProcessWindowStyle.Normal,
                Arguments = newInfo.FullName,
            };
            _process = Process.Start(startInfo);
            _process.WaitForExit(5000);
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
            Console.WriteLine(e);
        }
        finally
        {
            _process?.Kill();
        }

        return 0;
    }

    private static void InsertVba(ExcelWorkbook workbook)
    {
        workbook.CreateVBAProject();
        var project = workbook.VbaProject;
        if (Directory.Exists("modules"))
        {
            foreach (string file in Directory.GetFiles("modules"))
            {
                Console.WriteLine($"Module path {file}");
                var fileInfo = new FileInfo(file);
                using var ss = fileInfo.OpenText();
                string nameNotExt = fileInfo.Name.Split('.')[0];
                Console.WriteLine($"adding module: {nameNotExt}");
                if (nameNotExt == "Salesforce")
                {
                    Console.WriteLine("Removing duplicated salesforce module");
                    project.Modules.Remove(project.Modules["Salesforce"]);
                }
                var module = project.Modules.AddModule(nameNotExt);
                module.Code = ss.ReadToEnd();
            }
        }
    } 
}