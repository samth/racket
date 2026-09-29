# List every iconv DLL that LoadLibraryW could find by bare name,
# in search order, with the C runtime each one imports.
$names = 'iconv-2.dll', 'libiconv-2.dll', 'iconv.dll', 'libiconv.dll'
$dirs = @($args) + @("$env:SystemRoot\System32", $env:SystemRoot, (Get-Location).Path) +
        ($env:PATH -split ';' | Where-Object { $_ })
$dumpbin = Get-Command dumpbin.exe -ErrorAction SilentlyContinue
foreach ($d in $dirs) {
  foreach ($n in $names) {
    $f = Join-Path $d $n
    if (Test-Path -LiteralPath $f) {
      $crt = ''
      if ($dumpbin) {
        $crt = (& dumpbin.exe /nologo /dependents $f | Select-String -Pattern '^\s+\S+\.dll' |
                ForEach-Object { $_.Line.Trim() }) -join ' '
      }
      Write-Host "FOUND $f  [$crt]"
    }
  }
}
