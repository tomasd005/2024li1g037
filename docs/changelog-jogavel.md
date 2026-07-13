# Immutable Towers - Changelog Jogavel

Tags: #changelog #qa

Registo curto das mudancas com impacto real na experiencia de jogo.

Relacionadas:

- [[HOME]]
- [[estado-atual]]
- [[backlog-jogo]]

## 2026-07-13

### Ondas, inimigos e UI

- oito classes normais de inimigos e tres bosses com modelos proprios
- armadura, resistencias, escudo, regeneracao e fases simples
- Guardiao protege aliados; Ruptura cria uma zona que enfraquece torres
- vagas declarativas por grupos e tres mutadores combinaveis no infinito
- preview da proxima vaga no painel lateral
- corrigida a chegada a base em velocidades altas, incluindo inimigos que atravessam a base entre frames
- editor impede alteracoes que bloqueiem o caminho
- painel lateral reorganizado e alinhado com a hitbox real
- testes antigos limpos de instancias orfas e funcoes parciais
- suite atual com 157 testes a passar
- submenus passaram a usar `Voltar` com retorno por rato e teclado
- ecras de Opcoes, Loja, Perfil, Ranking e Ajuda ficaram com espacamento consistente

### Torres e progressao

- identidade explicita por torre construida, sem depender do tipo de projetil
- loja, painel e modelos ligados a uma configuracao unica por `TowerId`
- niveis maximos diferentes por torre
- painel mostra nivel, DPS aproximado, prioridade e refund
- formas e cores base distintas para as nove torres

### Saves e qualidade

- save de partida versionado com identidade, nivel e especializacao
- migracao automatica de saves antigos
- migracao e identidade cobertas por testes automaticos

## 2026-07-09

### UI e layout

- loja movida do rodape para sidebar lateral esquerda
- barra inferior aliviada para devolver area util ao mapa
- hitboxes da UI alinhadas com o render para bloquear cliques no mapa por baixo

### Torre e upgrade

- preview de upgrade adicionado ao painel lateral
- painel da torre com mais contexto: raridade, efeito, valor de venda e comparacao mais completa
- modelos das torres com melhor progressao visual por poder e por tier aproximado

### Combate

- bug de inimigos presos apos certos efeitos corrigido com separacao entre velocidade base e velocidade efetiva
- impacto visual de disparo reforcado
- numeros flutuantes simples de dano adicionados

### Documentacao

- vault do Obsidian estruturado com Home, estado atual, backlog, feedback e notas tecnicas por sistema

## Como usar esta nota

Quando houver uma nova ronda de alteracoes:

1. adicionar uma nova data
2. listar apenas mudancas sentidas pelo jogador ou pelo tester
3. evitar meter detalhes internos de implementacao aqui
