# Immutable Towers - Sistema Inimigos

Tags: #sistema #inimigos

Relacionadas: [[HOME]], [[estado-atual]], [[sistema-ondas]], [[backlog-jogo]]

## Modulos

- `app/EnemySystem.hs`: classes, configuracao, resistencias, passivos e bosses
- `app/WaveSystem.hs`: composicao das vagas
- `lib/Tarefa3.hs`: update, movimento, spatial grid e injecao das regras da app
- `app/Desenhar.hs`: modelos, estados, barras e auras

## Modelo compativel

O record academico `Inimigo` foi preservado. A classe e inferida por bandas estaveis de `velocidadeBaseInimigo`, enquanto vida, ataque e recompensa escalam por nivel. A velocidade efetiva e recalculada a partir da base antes dos efeitos, impedindo freezes permanentes.

## Classes

| Classe | Leitura | Counter principal |
|---|---|---|
| Basico | enxame regular | Sentinela/Braseiro |
| Rapido | pouca vida, muita velocidade | Glaciar |
| Tanque | vida e dano elevados | Impacto/Venenoide |
| Blindado | armadura e resistencia direta | dano prolongado/area |
| Regenerador | recupera vida | burst e foco de vida |
| Dispersor | resiste a area | single-target |
| Protegido | escudo acima de 66% de vida | quebrar escudo e focar |
| Elite | forte, rapido e com fase de velocidade | composicao mista |

## Bosses

- Ariete Veloz: acelera em duas fases de vida.
- Bastiao Vivo: armadura/escudo e aura que reduz o dano recebido por aliados proximos.
- Nexo da Ruptura: regenera, resiste a area e cria uma zona que reduz o dano das torres dentro dela.

As auras sao desenhadas com circulos pulsantes para comunicar alcance sem depender apenas da cor.

## Regras de dano

1. parte do dano e absorvida por armadura plana;
2. aplica-se resistencia direta ou de area conforme a torre;
3. o escudo pode reduzir o dano enquanto a vida esta alta;
4. auras de boss ajustam o dano final;
5. o projetil/estado da torre continua a ser aplicado.

## Performance

- torres consultam candidatos atraves de spatial grid, evitando comparar todas as torres com todos os inimigos
- classes e specs sao valores puros e pequenos
- o contexto de combate recolhe Guardioes e Rupturas uma vez por frame em O(n)
- cada impacto consulta apenas as posicoes dos bosses ativos, O(b), em vez de percorrer a vaga inteira

## Chegada a base

O movimento guarda a posicao inicial e verifica o segmento percorrido no frame contra a base. A celula da base tambem e uma zona terminal, para que a escolha de direcao nao possa inverter o inimigo na ultima celula do caminho. Se a velocidade ou o fast-forward fizerem o inimigo passar pela base sem ficar exatamente no centro dela, a posicao e ajustada para a base antes da separacao de estados. Assim o inimigo causa dano uma vez e nao pode inverter a rota por ter ultrapassado o alvo.

Esta regra esta coberta por um teste de regressao com um inimigo a atravessar a base num unico frame.

## Aberto

- IDs unicos apenas se forem necessarios para telemetria individual
- segunda fase de rota para o Nexo da Ruptura
- playtest dos counters e das auras
