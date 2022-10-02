try
{
    $ourDir = ".\Foundation\"

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
}
catch
{
    exit -1
}