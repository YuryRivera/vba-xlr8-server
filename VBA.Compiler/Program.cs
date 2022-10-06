using System.Diagnostics;
using OfficeOpenXml;
using OfficeOpenXml.VBA;
using VBA.Compiler.Package;
using VBA.Compiler.Responses;

namespace VBA.Compiler;

public static class Program
{
    public static int Main(string[] args)
    {
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

                Directory.CreateDirectory("Vshit");
                Directory.CreateDirectory("unknown");
                foreach (var module in vba.Modules)
                {
                    Console.WriteLine($"{module.Name}:");
                    foreach (var attribute in module.Attributes)
                    {
                        Console.Write($"{attribute.Name}={attribute.Value}");
                    }

                    string folder = module.Type switch
                    {
                        eModuleType.Document => "Vobjects",
                        eModuleType.Module => "Vmodules",
                        eModuleType.Class => "Vclasses",
                        eModuleType.Designer => "Vshit",
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
            package.Save();
            var startInfo = new ProcessStartInfo(@"C:\Program Files\Microsoft Office\Office15\EXCEL.EXE")
            {
                WindowStyle = ProcessWindowStyle.Normal,
                Arguments = path,
            };
            Process proc = Process.Start(startInfo);
            proc.WaitForExit();

            // StreamReader stream = File.OpenText("Objects/Aurora.vba");
            // string vbaSource = stream.ReadToEnd();
            //
            // package.Workbook.CreateVBAProject();
            // var project = 
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
            Console.WriteLine(e);
        }

        return 0;
    }
}