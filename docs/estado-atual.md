# Immutable Towers - Estado Atual

Tags: #estado #hub

Resumo vivo do estado jogavel. Detalhes tecnicos ficam nas notas de sistema e trabalho aberto fica apenas em [[backlog-jogo]].

Relacionadas: [[HOME]], [[extras-implementados]], [[roadmap-atual]]

## Ultima atualizacao

- Data: 2026-07-13
- Build: OK
- Testes: 157/157

## Estado geral

O jogo esta jogavel com menu, perfil e ranking locais, cinco modos, cinco mapas rotativos, campanha, loja, upgrades, classes de inimigos, efeitos, save/load, editor de mapa e bot automatico opcional.

## Torres

- nove identidades configuradas numa unica fonte `TowerSpec`
- `TowerId`, nivel e especializacao guardados explicitamente por torre construida
- prioridades reais: primeiro na rota, mais rapido, mais vida e maior grupo
- nivel maximo diferente por torre
- escolha permanente entre especializacao de dano/alcance e rajada/cadencia
- painel com DPS aproximado, stats, prioridade, custo, preview e refund
- modelos, cores e evolucao visual ligados a identidade runtime

Ver [[sistema-torres]].

## Inimigos e combate

- classes: basico, rapido, tanque, blindado, regenerador, dispersor, protegido e elite
- bosses: Ariete Veloz, Bastiao Vivo e Nexo da Ruptura
- armadura, resistencia direta/area, escudo, regeneracao e fases simples
- Guardiao protege aliados proximos; Ruptura enfraquece torres dentro da aura
- velocidade base separada da velocidade efetiva, evitando inimigos presos apos gelo/resina
- chegada a base detetada mesmo quando um inimigo atravessa a posicao da base entre frames
- spatial grid mantem a procura de alvos escalavel
- impactos, estados, barras de vida e numeros de dano visiveis

Ver [[sistema-inimigos]].

## Ondas, modos e mapas

- composicoes declarativas por grupos de classes
- historia com dez vagas por estagio e cinco estagios por capitulo
- infinito gera vagas continuamente e aplica Fortificados, Butim Reduzido e Onda Dupla em marcos fixos
- desafio, boss e sandbox usam composicoes e economias proprias
- preview textual da proxima vaga no painel lateral
- editor rejeita alteracoes que removam o caminho portal-base, movam o piso da base/portal ou invalidem torres
- cinco mapas rotativos com terreno normal, asfalto e agua

Ver [[sistema-ondas]].

## UI e input

- espaco virtual 1920x1080 com escala para varias resolucoes
- HUD superior, loja lateral esquerda e painel contextual direito
- painel direito usa a mesma `gamePanelRect` para desenho e bloqueio de input
- cliques sobre UI nao constroem torres no mapa
- troca de torre selecionada nao dispara construcao acidental
- pausa, velocidades 1x/2x/4x, HUD recolhivel e loja recolhivel
- ecras dedicados de vitoria e derrota
- submenus com fundo animado consistente e botoes `Voltar` clicaveis

Ver [[sistema-ui]].

## Progressao e persistencia

- perfil, ranking, gemas, baus, desbloqueios e fusao da Tempestade
- modos com nivel minimo
- save de partida versionado conserva identidade, nivel e especializacao
- saves antigos contendo apenas `Jogo` continuam a abrir por migracao de fallback

## Qualidade confirmada

- `cabal build`: OK
- `cabal test`: 157 casos, 0 erros, 0 falhas
- testes cobrem identidade, targeting, especializacoes, saves, resistencias, bosses, mutadores, editor e hitboxes
- instancias de igualdade e testes academicos foram limpos sem warnings orfaos ou funcoes parciais
- validacao visual manual desta ultima ronda ainda esta pendente
