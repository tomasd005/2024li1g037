# Immutable Towers - Distribuicao

Tags: #release #bundle

## Estrutura recomendada

```text
ImmutableTowers/
  immutable-towers.exe
  app/
    imagens/
      *.bmp
  README.md
```

O jogo carrega assets com caminhos relativos como `app/imagens/...`, por isso a pasta `app/imagens` deve ficar ao lado do executavel.

## Build local

```powershell
cabal build -O2
```

O executavel fica dentro de `dist-newstyle`. Para distribuir, copiar o executavel final e a pasta `app/imagens`.

## Windows

Recomendado:

1. Compilar com a mesma arquitetura do sistema alvo.
2. Copiar o executavel e `app/imagens`.
3. Testar numa pasta limpa fora do projeto.
4. Se o computador alvo nao tiver as DLLs necessarias, empacotar tambem as DLLs do runtime usadas por GHC/Gloss/OpenGL/GLUT.

Durante desenvolvimento, usa:

```powershell
.\run-game.ps1
```

Ou faz duplo clique em:

```text
run-game.bat
```

Isto adiciona `C:\ghcup\msys64\mingw64\bin` ao `PATH`, onde normalmente esta `libfreeglut.dll`.

## Linux

Recomendado:

1. Compilar no proprio Linux alvo ou num ambiente compativel.
2. Garantir bibliotecas OpenGL/GLUT instaladas.
3. Distribuir binario + `app/imagens`.

## macOS

Recomendado:

1. Compilar em macOS.
2. Criar uma pasta `.app` apenas numa fase posterior.
3. Garantir que as bibliotecas nativas usadas por Gloss estao presentes.

## Pontos de risco

- Gloss depende de OpenGL/GLUT, que varia por sistema operativo.
- Caminhos relativos dos assets devem ser preservados.
- Fontes TTF ainda nao estao integradas; a UI atual usa texto vetorial do Gloss.
