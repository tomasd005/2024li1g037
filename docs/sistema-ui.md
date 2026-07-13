# Immutable Towers - Sistema UI

Tags: #sistema #ui

Relacionadas: [[HOME]], [[estado-atual]], [[backlog-jogo]]

## Modulos

- `app/Desenhar.hs`: composicao visual
- `app/UIComponents.hs`: paineis, botoes e `UIRect`
- `app/UIRects.hs`: geometria interativa partilhada
- `app/UIState.hs`: dados derivados para HUD e vagas
- `app/UIText.hs`: tipografia bitmap/Gloss
- `app/Eventos.hs`: input e bloqueio da UI
- `lib/MapGeometry.hs`: conversao mapa/ecra

## Layout atual

- espaco virtual base de 1920x1080, escalado uniformemente para a janela
- HUD e controlos no topo
- loja recolhivel na esquerda
- painel contextual recolhivel na direita
- mapa ocupa o centro sem receber cliques atraves dos paineis

## Fonte de verdade

`UIRects.hs` define as areas desenhadas e interativas. O painel lateral usa `gamePanelRect` tanto no render como em `cliqueBloqueadoPelaUI`; os botoes de upgrade, especializacao, venda e limpar sao testados para permanecer dentro desse painel.

O painel esta dividido em faixas fixas:

1. mapa, capitulo e preview da proxima vaga;
2. estado da base/partida;
3. torre selecionada e comparacao do upgrade;
4. acoes.

## Estados e acessibilidade

- hover, selecionado, desativado e dinheiro insuficiente
- contraste por texto e contorno, nao apenas cor
- pausa, 1x/2x/4x, HUD e loja recolhiveis
- vitoria e derrota com fluxo proprio
- submenus com fundo animado comum e botoes `Voltar` ligados a hitboxes partilhadas

## Aberto

- validar visualmente todas as resolucoes alvo
- adicionar icones no preview e barra de progresso da vaga
- categorias/scroll quando o roster ultrapassar o espaco da sidebar
- reducao de efeitos e opcoes de video reais
