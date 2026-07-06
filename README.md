# Laboratórios de Informática I

## Executável

Pode compilar e executar o programa através dos comandos `build` e `run` do Cabal.

```bash
cabal run --verbose=0
```

### Windows / GHCup

Se ao executar aparecer o erro `unknown GLUT entry glutInit`, falta o DLL do
FreeGLUT no `PATH`. Instale a dependência pelo MSYS2 do GHCup:

```bash
/c/ghcup/msys64/usr/bin/pacman.exe -Sy --noconfirm mingw-w64-x86_64-freeglut
```

Depois execute com o script que adiciona o FreeGLUT ao `PATH` só para este
comando:

```bash
bash scripts/run-windows.sh
```

## Interpretador

Para abrir o interpretador do Haskell (GHCi) com o projeto carregado, utilize o comando `repl` do Cabal

```bash
cabal repl
```

## Testes

O projecto utiliza a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit) para fazer testes unitários.

Execute os testes com o comando `test` do Cabal e utilize a flag `--enable-coverage` para gerar um relatório de cobertura de testes.

```bash
cabal test --enable-coverage
```

Execute os exemplos da documentação como testes com a biblioteca
[`doctest`](https://hackage.haskell.org/package/doctest). Para instalar o
executavel utilize o comando `cabal install doctest`.

```bash
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest --verbose=0
```

## Documentação

A documentação do projeto pode ser gerada recorrendo ao [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock
```
