# Immutable Towers - Sistema Ondas

Tags: #sistema #ondas

Relacionadas: [[HOME]], [[estado-atual]], [[sistema-inimigos]], [[backlog-jogo]]

## Modulos

- `app/WaveSystem.hs`: `EnemyGroup`, `WavePlan`, composicoes e mutadores
- `app/ProgressionSystem.hs`: campanha, desbloqueios e total de vagas
- `app/Tempo.hs`: spawn automatico e geracao do infinito
- `app/UIState.hs`: resumo e composicao da proxima vaga

## Composicao

Uma `WavePlan` descreve grupos de classes e ciclo de spawn. Isto permite desenhar historia, desafio, boss e sandbox sem depender apenas de `indice mod n`.

- Historia: dez vagas por estagio, dificuldade guiada pelo capitulo/estagio.
- Infinito: uma vaga de cada vez, quantidade e nivel crescentes.
- Desafio: misturas que exigem counters diferentes.
- Boss: tres encontros com escoltas e bosses distintos.
- Sandbox: amostra das classes normais para teste.

## Mutadores do infinito

- `Fortificados`: a cada quatro vagas, mais vida e ataque.
- `RecompensaEscassa`: a cada seis vagas, menos butim.
- `OndaDupla`: a cada nove vagas, duplica a composicao e acelera o spawn.

Os marcos podem coincidir, criando combinacoes previsiveis. O HUD mostra uma notificacao quando um mutador entra.

## Preview

`UIState` agrega a composicao da proxima vaga numa passagem e o painel mostra ate tres classes com contagens. O calculo fica fora das funcoes de desenho de modelos e evita multiplas pesquisas por classe.

## Editor

Cada alteracao e validada antes de ser aplicada. O editor rejeita:

- base ou portal fora de Terra/Asfalto;
- terreno invalido sob uma torre;
- mapa sem caminho de qualquer portal ate a base.

## Qualidade

- ciclos de spawn sao limitados a valores positivos
- mutadores e composicoes possuem testes unitarios
- falta playtest de ritmo, pausas e economia em vagas longas
