$ErrorActionPreference = "Stop"
$ReleaseRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $ReleaseRoot
& (Join-Path $ReleaseRoot "immutable-towers.exe")
