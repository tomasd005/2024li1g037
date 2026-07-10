# Immutable Towers - Novas Features

Tags: #roadmap #features

## Pedido original

- implementar ecras de derrota e vitoria com melhor fluxo
- corrigir a aba de opcoes e adicionar informacao de resolucao/graficos
- melhorar a legibilidade do texto dentro do jogo
- transformar o bot numa opcao automatica e nao apenas numa acao isolada

## Ja implementado

- derrota e vitoria agora deixaram de ficar num overlay passivo:
  - derrota abre um ecran de resultado com estatisticas da partida
  - vitoria abre um ecran de resultado com estatisticas e escolha entre menu ou repetir nivel
- o resumo da partida mostra:
  - modo
  - mapa
  - tempo jogado
  - pontuacao
  - ondas
  - creditos finais
  - torres no mapa
- o `ESC` deixou de abrir a pausa nesses estados finais:
  - derrota volta para o menu
  - vitoria volta para o menu
  - `ENTER` na vitoria repete o nivel
- foi adicionada a opcao `AUTO` no topo durante a partida
- o bot automatico agora consegue:
  - iniciar vaga
  - construir torre
  - melhorar torre
- o antigo botao manual `GO` foi removido da barra superior
- as vagas passam a arrancar automaticamente sem precisar desse botao
- o HUD do jogo passou a mostrar o estado do bot (`AUTO` ou `MANUAL`)
- os pills principais do HUD passaram a usar a fonte bitmap do jogo, ficando mais legiveis
- o menu de opcoes ja mostra:
  - resolucao atual
  - perfil grafico base
  - controlo do bot automatico nos atalhos

## Ainda por fechar

- limpar o texto antigo com encoding estranho em alguns menus de opcoes
- substituir mais texto do jogo pela fonte bitmap, nao apenas o HUD principal
- evoluir as opcoes de graficos de informativas para realmente configuraveis
- validar em playtest se o fluxo de vitoria/derrota esta confortavel
