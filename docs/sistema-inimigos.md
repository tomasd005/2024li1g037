# Immutable Towers - Sistema Inimigos

Tags: #sistema #inimigos

Relacionadas:

- [[HOME]]
- [[estado-atual]]
- [[backlog-jogo]]

## Responsabilidades

- spawn por ondas
- movimento no mapa
- aplicacao de efeitos
- dano na base
- recompensa ao morrer

## Modulos principais

- `app/WaveSystem.hs`
- `lib/Tarefa3.hs`
- `lib/Tarefa2.hs`
- `app/Desenhar.hs`

## Estado atual

- inimigos podem receber fogo, gelo, resina, medo, veneno e eletrico
- ha inimigos rapidos e inimigos brutos nas ondas
- a velocidade base ficou separada da velocidade efetiva

## Correcao importante recente

- foi introduzida `velocidadeBaseInimigo`
- isto evita que gelo/resina deixem o inimigo preso de forma permanente

## Feedback visual

- barra de vida por inimigo
- efeitos visuais por estado
- impacto visual de disparo
- numero de dano flutuante simplificado junto do alvo

## Proximos melhoramentos

- testar mais combinacoes de efeitos em playtest
- considerar IDs unicos de inimigos se for preciso rastrear dano de forma mais rica
