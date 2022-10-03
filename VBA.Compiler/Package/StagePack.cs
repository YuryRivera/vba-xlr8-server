namespace VBA.Compiler.Package;

public class StagePack
{
    public void CopyXmlFiles()
    {
        string tempDir = Path.GetTempPath();
        string cwd = Directory.GetCurrentDirectory();
        string pathName = cwd.Split('\\')[^1];
        Console.WriteLine(pathName);
        string tempPath = Path.Join(tempDir, pathName);
        Directory.CreateDirectory(tempPath);
        CopyDir("_rels", tempPath);
        CopyDir("docProps", tempPath);
        CopyDir("xl", tempPath);
        File.Copy("[Content_Types].xml", Path.Combine(tempPath, "[Content_Types].xml"), true);
    }

    private void CopyDir(string path, string temp)
    {
        string currentTemp = Path.Combine(temp, path);
        Directory.CreateDirectory(currentTemp);
        foreach (string filePath in Directory.GetFiles(path))
        {
            string fName = Path.GetFileName(filePath);
            Console.WriteLine("Copying {0} {1}", currentTemp, fName);
            File.Copy(filePath, Path.Combine(currentTemp, fName));
        }
    }
}