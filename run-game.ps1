$ErrorActionPreference = "Stop"

$ProjectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$GlutBin = "C:\ghcup\msys64\mingw64\bin"
$Exe = Join-Path $ProjectRoot "dist-newstyle\build\x86_64-windows\ghc-9.10.3\immutable-towers-0.0.0.0\x\immutable-towers\build\immutable-towers\immutable-towers.exe"

if (-not (Test-Path $Exe)) {
  Write-Host "Executavel nao encontrado. A correr cabal build..."
  Push-Location $ProjectRoot
  cabal build
  Pop-Location
}

if (-not (Test-Path $GlutBin)) {
  Write-Host "GLUT nao encontrado em $GlutBin"
  Write-Host "Instala/atualiza o ambiente MSYS2 do GHCup ou abre o jogo com cabal run."
  Read-Host "Pressiona ENTER para sair"
  exit 1
}

$env:PATH = "$GlutBin;$env:PATH"
Set-Location $ProjectRoot
& $Exe

