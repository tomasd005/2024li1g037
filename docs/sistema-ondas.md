# Sistema Ondas

Relacionadas:

- [[HOME]]
- [[estado-atual]]

## Responsabilidades

- criar ondas
- escalar dificuldade
- gerir spawn de inimigos
- suportar modos de jogo diferentes

## Modulos principais

- `app/WaveSystem.hs`
- `lib/Tarefa3.hs`
- `app/UIState.hs`
- `app/Tempo.hs`

## Estado atual

- historia usa ondas mais guiadas
- infinito gera novas ondas dinamicamente
- desafio, boss e sandbox usam configuracoes diferentes

## UI ligada ao sistema

- HUD mostra vaga atual
- HUD mostra inimigos restantes
- UIState calcula resumo das ondas para o render

## Proximos melhoramentos

- variedade maior de composicao de ondas
- bosses com comportamento proprio
- tuning do modo infinito para escalar melhor no late game
