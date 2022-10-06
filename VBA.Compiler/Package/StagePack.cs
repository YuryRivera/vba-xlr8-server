using System.IO.Compression;

namespace VBA.Compiler.Package;

public class StagePack
{
    private const string StageName = "vba-xlr8";

    public void CopyXmlFiles()
    {
        string tempDir = Path.GetTempPath();
        string cwd = Directory.GetCurrentDirectory();
        string pathName = cwd.Split('\\')[^1];
        Console.WriteLine(pathName);
        string tempPath = Path.Join(tempDir, StageName, pathName, "src");
        Directory.CreateDirectory(tempPath);
        Directory.Delete(Path.Combine(tempDir, StageName), true);
        CopyDir("_rels", tempPath);
        CopyDir("docProps", tempPath);
        CopyDir("xl", tempPath);
        File.Copy("[Content_Types].xml", Path.Combine(tempPath, "[Content_Types].xml"), true);
    }

    public string Compress()
    {
        string tempDir = Path.GetTempPath();
        string cwd = Directory.GetCurrentDirectory();
        string pathName = cwd.Split('\\')[^1];
        string tempPath = Path.Join(tempDir, StageName, pathName, "src");
        string filePath = Path.Join(tempDir, StageName, pathName, $"{pathName}.xlsm");
        Console.WriteLine($"Compressing {filePath}");
        ZipFile.CreateFromDirectory(tempPath, filePath, CompressionLevel.NoCompression, false);
        return filePath;
    }

    private static void CopyDir(string path, string temp)
    {
        string currentTemp = Path.Combine(temp, path);
        Directory.CreateDirectory(currentTemp);
        foreach (string filePath in Directory.GetFiles(path))
        {
            string fName = Path.GetFileName(filePath);
            Console.WriteLine("Copying {0} {1}", currentTemp, fName);
            File.Copy(filePath, Path.Combine(currentTemp, fName), true);
        }

        foreach (string directory in Directory.GetDirectories(path))
        {
            CopyDir(directory, temp);
        }
    }
}