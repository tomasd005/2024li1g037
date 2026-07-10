# Immutable Towers - Checklist de Regressao

Tags: #qa #checklist

Checklist rapida para validar o jogo antes de bundle, commit importante ou entrega.

Relacionadas:

- [[HOME]]
- [[estado-atual]]
- [[changelog-jogavel]]

## Menu

- abrir o jogo e confirmar que o menu principal aparece bem
- navegar entre botoes com rato e teclado
- abrir perfil, ranking, ajuda, opcoes e modos
- confirmar que o texto principal esta legivel

## Perfil e progresso

- editar nome do perfil
- usar backspace
- confirmar que os dados persistem ao reabrir

## Inicio de partida

- entrar numa partida de historia
- entrar numa partida de infinito
- confirmar que o mapa carrega e que a UI nao sai do ecran

## Loja e construcao

- selecionar uma torre na sidebar
- trocar para outra sem construir acidentalmente
- clicar em zonas de UI e confirmar que nao passa input para o mapa
- colocar torre em relva valida
- tentar colocar torre em zona invalida

## Torre e upgrade

- selecionar torre colocada
- ver stats no painel lateral
- confirmar preview de upgrade
- fazer upgrade
- confirmar feedback visual do upgrade
- vender torre

## Combate

- confirmar que as vagas arrancam automaticamente
- confirmar que projeteis aparecem
- confirmar que inimigos perdem vida
- confirmar que aparecem numeros de dano
- confirmar que inimigos com gelo/resina nao ficam presos para sempre

## Modos e velocidade

- trocar entre 1x, 2x e 4x
- pausar e retomar
- testar HUD on/off
- testar loja on/off

## Save/Load

- guardar jogo
- carregar jogo
- confirmar que o estado principal volta corretamente

## Build

- correr `cabal build`
- correr `cabal test`
- confirmar que a suite continua verde
