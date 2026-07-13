# Immutable Towers

Tower defense 2D desenvolvido em Haskell com Gloss. Inclui campanha, modos alternativos, cinco mapas, nove torres, classes de inimigos, bosses, progressao local, loja, upgrades ramificados, save/load, editor de mapas e bot opcional.

## Executar no Windows

Durante o desenvolvimento:

```powershell
.\run-game.ps1
```

Ou atraves do Cabal:

```powershell
cabal run --verbose=0
```

Se surgir `unknown GLUT entry glutInit`, instala FreeGLUT no ambiente MSYS2/GHCup. O script `scripts/run-windows.sh` prepara o `PATH` para essa instalacao.

## Controlos principais

- rato: menus, comprar, construir, selecionar, melhorar e vender torres
- `P` ou `Esc`: pausar/continuar
- `X`: alternar velocidade entre 1x, 2x e 4x
- `H`: recolher/mostrar o painel de informacao
- `K`: esconder/mostrar a loja lateral
- `S` / `L`: guardar/carregar a partida
- `A`: ativar/desativar o bot automatico
- `E`: abrir o editor de mapas a partir do menu

## Sistemas principais

- nove torres com identidade explicita, nivel maximo, prioridade de alvo e duas especializacoes de late game
- onze classes de inimigos, incluindo rapido, tanque, blindado, regenerador, protegido, elite e tres bosses
- ondas declarativas e mutadores graduais no modo infinito
- historia com capitulos, cinco estagios por capitulo e dez vagas por estagio
- perfil, ranking, gemas, baus, desbloqueios e fusao da Tempestade
- save versionado com migracao automatica de saves antigos

## Desenvolvimento

```powershell
cabal build
cabal test
```

O codigo esta separado entre a API academica em `lib/`, os sistemas da aplicacao em `app/`, testes em `test/` e documentacao/Obsidian em `docs/`.

## Distribuicao

O bundle Windows deve incluir o executavel, as DLL necessarias e `app/imagens`. Consulta `docs/distribuicao.md` e usa:

```powershell
.\scripts\package-windows-release.ps1
```

## Documentacao

Abre `docs/` como vault no Obsidian e comeca em `HOME.md`. O estado jogavel, roadmap, backlog, checklist de regressao e notas tecnicas por sistema sao mantidos nesse vault.
