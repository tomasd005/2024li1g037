# Estado atual do jogo

Resumo vivo do estado jogavel atual do Immutable Towers.

Relacionadas:

- [[HOME]]
- [[extras-implementados]]
- [[backlog-jogo]]

## Ultima atualizacao

- Data: 2026-07-09

## Estado geral

O jogo encontra-se jogavel com menu principal, perfis locais, ranking local, varios modos, loja in-game, upgrades, efeitos de projetil, save/load e editor de mapa.

## Sistemas ativos

### Gameplay

- ondas de inimigos
- torres com varios tipos de projetil
- efeitos: fogo, gelo, resina, medo, veneno, eletrico
- sinergias entre projeteis
- upgrades de torres
- venda de torres
- modos historia, infinito, desafio, boss e sandbox

### Progressao e meta

- perfil local
- leaderboard local
- gemas
- desbloqueio de torres
- baus
- fusao para torre Tempestade
- progresso por capitulo/estagio

### Ferramentas

- save/load local
- editor de mapa
- sugestao/bot simples
- bundle Windows

## Implementacao mais recente

### Input e UI

- a UI agora bloqueia corretamente o clique no mapa por baixo
- trocar de torre na loja ja nao deve construir acidentalmente no terreno
- as zonas de HUD, loja e painel lateral passaram a consumir input
- a loja in-game passou para uma sidebar lateral esquerda em vez de ocupar o rodape
- a barra inferior ficou mais leve para devolver leitura ao mapa
- o layout foi validado em testes com resolucoes representativas: 1280x720, 1600x900, 1920x1080 e 2560x1440

### Upgrades

- o painel da torre mostra preview do upgrade antes da compra
- o upgrade agora mostra melhor o ganho esperado em stats
- foi adicionado feedback visual no momento da melhoria
- os modelos das torres agora ganham detalhes visuais conforme o poder/upgrade
- o painel lateral da torre passou a mostrar mais contexto: raridade, efeito, venda e comparacao de upgrade mais completa

### Inimigos

- corrigida a logica de velocidade para separar velocidade base de efeitos temporarios
- isto resolve o bug em que alguns inimigos ficavam presos apos certos disparos
- os disparos agora deixam um impacto visual mais claro no alvo
- o feedback de dano ficou mais explicito com numeros flutuantes simples no inimigo atingido

## Estado dos testes

- `cabal build`: OK
- `cabal test`: OK
- suite: 130/130

## Notas de manutencao

Esta nota deve manter:

- o que esta funcional agora
- o que foi corrigido recentemente
- o estado atual de build/testes

## Documentacao tecnica

Ja existem notas por sistema no vault:

- [[sistema-ui]]
- [[sistema-torres]]
- [[sistema-inimigos]]
- [[sistema-ondas]]
- [[sistema-progressao]]
