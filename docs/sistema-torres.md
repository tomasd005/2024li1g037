# Immutable Towers - Sistema Torres

Tags: #sistema #torres

Relacionadas: [[HOME]], [[estado-atual]], [[backlog-jogo]]

## Modulos

- `app/TowerSystem.hs`: `TowerSpec`, loja, targeting, upgrades e balanceamento
- `app/TowerRuntime.hs`: identidade, nivel e especializacao por celula
- `app/Eventos.hs`: comprar, construir, melhorar, especializar e vender
- `app/Desenhar.hs`: modelos e painel contextual
- `app/SaveSystem.hs`: persistencia e migracao
- `lib/Tarefa3.hs`: combate generico com seletor e resolvedor injetados

## Arquitetura

`LI12425.Torre` permanece compativel com a API academica. A aplicacao associa cada torre a um `TowerRuntime`:

- `TowerId`
- nivel atual
- especializacao opcional

A chave usa a celula inteira do mapa. `towerSpecDaTorre` consulta o registo explicito; `towerSpecAproximada` e apenas fallback para saves antigos e acoes legadas.

## Balanceamento base

| Torre | Papel | Alvo | Preco | Max |
|---|---|---:|---:|---:|
| Sentinela | dano consistente | primeiro | 44 | 4 |
| Glaciar | controlo | rapido | 62 | 5 |
| Braseiro | dano continuo | grupo | 74 | 5 |
| Panico | controlo de rota | primeiro | 92 | 5 |
| Venenoide | execucao | mais vida | 106 | 5 |
| Tesla | anti-enxame | grupo | 122 | 6 |
| Impacto | rajada pesada | mais vida | 138 | 6 |
| Solar | suporte ofensivo | primeiro | 146 | 6 |
| Tempestade | endgame | grupo | 188 | 7 |

## Especializacoes

Ao chegar ao ponto de escolha, o upgrade normal para ate o jogador selecionar:

- `DANO+`: mais 12% de dano e 0.18 de alcance nos upgrades seguintes, custo superior;
- `RAPIDA`: mais um alvo por rajada e ciclo 14% menor, com limite de seguranca.

A escolha e permanente, tem modelo visual proprio e e guardada no save.

## Targeting

- Primeiro na rota: menor distancia estimada a base.
- Rapido: maior velocidade efetiva.
- Mais vida: maior vida atual.
- Grupo: alvo mais proximo do centro dos candidatos.

A spatial grid limita primeiro os candidatos ao alcance; a ordenacao ocorre apenas nesse conjunto.

## Aberto

- playtest de preco/DPS no inicio, meio e late game
- telemetria por torre para balanceamento
- efeitos/model swaps mais fortes nos upgrades altos
