try
{
    $ourDir = ".\Foundation\"
    $libDir = ".\Lang\"
    $pack = "VBA.Parser.Foundation"
    $lexerFile = ".\Lang\VbaLexer.g4"

    if (Test-Path $ourDir)
    {
        Write-Host "Folder Exists"
    }
    else
    {
        New-Item $ourDir -ItemType Directory
        Write-Host "Folder Created successfully"
    }
    
    Remove-Item "$ourDir*" -Recurse -Force
    java -jar Scripts\antlr.jar -lib $libDir -Dlanguage=CSharp -encoding UTF8 -Xexact-output-dir -package $pack -o $libDir $lexerFile
    Move-Item "$libDir\VbaLexer.cs" -Destination $ourDir -Force

}
catch
{
    exit -1
}