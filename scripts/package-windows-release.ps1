$ErrorActionPreference = "Stop"

$ProjectRoot = Split-Path -Parent $PSScriptRoot
$ReleaseRoot = Join-Path $ProjectRoot "release\windows"
$ExeSource = Join-Path $ProjectRoot "dist-newstyle\build\x86_64-windows\ghc-9.10.3\immutable-towers-0.0.0.0\x\immutable-towers\build\immutable-towers\immutable-towers.exe"
$AssetSource = Join-Path $ProjectRoot "app\imagens"
$MinGwBin = "C:\ghcup\msys64\mingw64\bin"
$DllCandidates = @(
  "libfreeglut.dll"
)

if (-not (Test-Path $ExeSource)) {
  throw "Executavel nao encontrado em $ExeSource. Corre cabal build primeiro."
}

if (Test-Path $ReleaseRoot) {
  Remove-Item -LiteralPath $ReleaseRoot -Recurse -Force
}

New-Item -ItemType Directory -Path $ReleaseRoot | Out-Null
New-Item -ItemType Directory -Path (Join-Path $ReleaseRoot "app") | Out-Null

Copy-Item -LiteralPath $ExeSource -Destination (Join-Path $ReleaseRoot "immutable-towers.exe")
Copy-Item -LiteralPath $AssetSource -Destination (Join-Path $ReleaseRoot "app\imagens") -Recurse

foreach ($dll in $DllCandidates) {
  $source = Join-Path $MinGwBin $dll
  if (Test-Path $source) {
    Copy-Item -LiteralPath $source -Destination (Join-Path $ReleaseRoot $dll)
  }
}

Write-Host "Release Windows criada em $ReleaseRoot"
