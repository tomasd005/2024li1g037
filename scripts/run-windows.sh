#!/usr/bin/env bash
set -euo pipefail

# Gloss usa GLUT em runtime. No Windows/GHCup, o DLL do FreeGLUT fica
# normalmente no MSYS2 incluído pelo GHCup e tem de estar no PATH.
GHCUP_MINGW_BIN="/c/ghcup/msys64/mingw64/bin"

if [ ! -f "$GHCUP_MINGW_BIN/libfreeglut.dll" ]; then
  cat >&2 <<'EOF'
FreeGLUT não encontrado.

Instala primeiro com:
  /c/ghcup/msys64/usr/bin/pacman.exe -Sy --noconfirm mingw-w64-x86_64-freeglut

Depois volta a executar:
  bash scripts/run-windows.sh
EOF
  exit 1
fi

PATH="$GHCUP_MINGW_BIN:$PATH" cabal run immutable-towers --verbose=0
