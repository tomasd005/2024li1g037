# Prompt Mestre — Continuidade e grande evolução do Immutable Towers

> Copia o texto abaixo para o próximo modelo/agente. Ele deve tratar este repositório como a fonte de verdade e trabalhar diretamente sobre o código.

## Prompt

Estás a continuar o desenvolvimento do jogo **Immutable Towers**, um tower defense em Haskell/OpenGL/Gloss para Windows. Trabalha como programador sénior, game designer e QA. O objetivo é fazer uma evolução grande, coerente e jogável do projeto, preservando a base funcional e melhorando bastante a experiência.

### 1. Antes de alterar qualquer coisa

Inspeciona completamente o repositório atual, incluindo:

- `README.md`, `immutable-towers.cabal`, scripts de execução e distribuição;
- toda a pasta `app/`, `lib/`, `test/` e `docs/`;
- especialmente `LI12425.hs`, `Tarefa1.hs`, `Tarefa2.hs`, `Tarefa3.hs`, `TowerSystem.hs`, `WaveSystem.hs`, `Tempo.hs`, `Eventos.hs`, `Desenhar.hs`, `UIRects.hs`, `UIState.hs`, `GameFactory.hs`, `ProgressionSystem.hs`, `MetaTypes.hs`, `SaveSystem.hs` e `ScoreSystem.hs`;
- estado do git, imagens existentes, limitações do `.cabal` e compatibilidade Windows.

Executa primeiro `cabal build` e `cabal test`. Não assumes que a documentação está perfeita: confirma no código o que está realmente implementado. Faz um diagnóstico curto com:

1. sistemas existentes;
2. bugs e riscos reais;
3. dívida técnica;
4. oportunidades de design;
5. plano priorizado.

Não reescrevas o projeto por impulso. Mantém a API académica e as funções públicas de `lib/` compatíveis sempre que possível. Se uma alteração de dados for necessária, migra os construtores, saves e testes de forma completa.

### 2. Leitura crítica do estado atual

Parte destas observações, mas verifica-as:

- o jogo já tem menu, perfil local, leaderboard, história, infinito, desafio, boss, sandbox, loja, upgrades, efeitos, sinergias, save/load, editor de mapa e bot automático;
- a UI usa um espaço virtual de `1920x1080`, sidebar de loja e painel lateral, com lógica de input em `Eventos.hs` e geometria em `UIRects.hs`/`MapGeometry.hs`;
- a `Torre` runtime ainda não guarda explicitamente o `TowerId`; a identidade é inferida pelo tipo de projétil e pelos stats, o que causa colisões semânticas entre torres que partilham projétil;
- `Loja` é uma lista de `(Creditos, Torre)`, portanto também perde identidade e dificulta tooltips, modelos, targeting e upgrades ramificados;
- os inimigos têm efeitos representados como projéteis ativos, mas o modelo ainda é limitado para resistências, armadura, vida máxima, classes, imunidades, IDs e bosses;
- as ondas são essencialmente geradas por nível/índice, faltando composição declarativa, telemetria e variedade controlada;
- o bot é útil como protótipo, mas tem heurísticas simples e deve continuar opcional;
- existem warnings antigos nos testes e textos com encoding estranho que devem ser limpos sem quebrar português.

### 3. Direção de design

Faz o jogo parecer um produto completo, não apenas uma demo técnica. Usa estes princípios:

- **clareza antes de complexidade**: o jogador deve entender por que perdeu e por que uma torre é boa;
- **papéis distintos**: cada torre deve ter função principal, fraquezas, alvo ideal e assinatura visual própria;
- **escolhas com custo de oportunidade**: comprar torre, fazer upgrade, poupar para uma especialização ou preparar a próxima vaga devem competir entre si;
- **counters legíveis**: inimigos devem ensinar o jogador a adaptar a composição;
- **variedade controlada**: introduz sistemas por camadas, com tutorialização e desbloqueios graduais;
- **feedback audiovisual económico**: impacto, estado, morte, upgrade e perigo devem ser reconhecíveis sem poluir o mapa;
- **replayability**: mapas, desafios, mutadores e ondas devem criar decisões diferentes, não apenas números maiores.

### 4. Melhorias de gameplay a implementar

Implementa, nesta ordem, o que for seguro e útil:

#### A. Identidade explícita e dados configuráveis

- introduz `TowerId` no estado runtime ou cria uma estrutura compatível que associe identidade à torre;
- separa `TowerSpec`/configuração de torre, estado runtime e apresentação visual;
- dá a cada torre: papel, descrição curta, cor, forma/silhueta, tags, prioridade de alvo, custo, alcance, dano, cadência, área, efeito e limites;
- substitui inferências frágeis como `towerSpecAproximada` onde isso for semanticamente perigoso;
- cria uma fonte de configuração única para loja, painel, render, progressão e balanceamento;
- preserva compatibilidade de saves antigos com uma migração segura ou fallback explícito.

#### B. Torre e progressão em profundidade

Mantém as torres atuais, mas torna-as mais distintas. Define papéis como:

- Sentinela: barata, consistente, single-target;
- Glaciar: controlo e criação de janelas de segurança;
- Braseiro: dano contínuo e limpeza de grupos;
- Pânico: controlo de rota/retirada, com risco de anti-sinergia;
- Venenoide: dano prolongado e execução de inimigos resistentes;
- Tesla: cadeia/área e resposta a enxames;
- Impacto: explosão/rajada pesada;
- Solar: suporte, buff ou dano de janela;
- Tempestade: fusão de alto investimento e identidade de endgame.

Adiciona uma decisão de especialização nos upgrades altos: duas opções mutuamente exclusivas, cada uma com benefício, custo e visual próprios. Mostra no painel:

- stats atuais e novos;
- dano por segundo aproximado quando fizer sentido;
- alcance, cadência, área e duração;
- alvo prioritário;
- descrição da habilidade;
- custo, refund de venda e impacto na economia;
- comparação visual clara antes da confirmação.

Não inventes números arbitrários sem justificar. Cria uma tabela de balanceamento e testa pelo menos início, meio e late game.

#### C. Inimigos, counters e bosses

Mantém os inimigos rápidos e brutos e acrescenta classes com leitura imediata, por exemplo:

- básico;
- rápido;
- bruto/tanque;
- blindado, reduzindo dano direto;
- regenerador ou curável;
- dispersor, resistente a área;
- protegido, exigindo quebrar escudo;
- elite com uma regra simples;
- boss com fases e telegráficos claros.

Modela vida máxima, resistência/armadura, tags, recompensa, velocidade base, velocidade efetiva e efeitos temporários sem permitir estados presos. Se adicionares IDs, usa-os para dano, estatísticas e efeitos sem depender de igualdade por posição.

Cria pelo menos três bosses verdadeiramente diferentes: um que acelera ou invoca, um que protege outros inimigos e um que altera a rota/zonas de perigo. O boss deve comunicar preparação, fase, ataque e perigo.

#### D. Ondas e economia

- substitui, onde fizer sentido, ondas geradas apenas por índice por composições declarativas;
- cria uma curva de dificuldade com orçamento de ameaça, mistura de classes, intervalos e respites;
- adiciona pré-visualização da próxima vaga com ícones e contagens;
- permite preparar a vaga sem retirar o controlo do jogador;
- garante que a economia recompensa bom targeting sem tornar uma torre dominante;
- analisa a escolha “nova torre vs upgrade” em vários momentos da partida;
- mantém infinito interessante através de mutadores graduais, não apenas HP multiplicado.

#### E. Modos e mapas

Melhora os modos existentes sem os tornar redundantes:

- História: ensino gradual e desbloqueios claros;
- Infinito: escalada, marcos e mutadores;
- Desafio: uma regra que muda a estratégia;
- Boss: encontro desenhado à volta do boss;
- Sandbox: ferramentas para testar, limpar e comparar.

Adiciona pelo menos dois mutadores reutilizáveis, por exemplo crédito inicial reduzido, asfalto expandido, inimigos camuflados, ondas duplas ou recompensas por não sofrer dano. O editor deve validar que o mapa continua jogável e não bloqueia o único caminho.

### 5. UI, UX e apresentação

- unifica render e hitboxes numa fonte de verdade;
- conserva a sidebar, mas prepara scroll/categorias se o roster crescer;
- usa tooltips e uma enciclopédia/almanac de torres e inimigos;
- mostra claramente o próximo evento perigoso e a razão de um inimigo ser resistente;
- dá estados de hover, selecionado, inválido, upgrade disponível e dinheiro insuficiente;
- melhora números de dano com cores/tipos, mas limita spam e sobreposição;
- adiciona barra de progresso da vaga, perigo da base, estado dos efeitos e feedback de habilidade;
- torna opções de vídeo realmente configuráveis apenas se for viável; caso contrário, expõe o que é real e remove promessas falsas;
- corrige encoding e uniformiza a tipografia em português;
- mantém acessibilidade básica: contraste, pausa, speed toggle, redução de efeitos e leitura de informação sem depender apenas de cor;
- valida 1280x720, 1600x900, 1920x1080 e 2560x1440.

### 6. Qualidade técnica obrigatória

- escreve testes unitários para identidade, targeting, efeitos, resistências, bosses, economia, upgrades, waves, save migration e hitboxes;
- adiciona testes de regressão para o bug de inimigos presos e para cliques da UI que não podem construir no mapa;
- evita `head`, partial functions e warnings novos;
- mantém estado numérico seguro: sem NaN, infinito acidental, ciclos <= 0 ou velocidades negativas;
- usa funções puras para regras de jogo e deixa IO limitado a save/load e ciclo da aplicação;
- não adiciona dependências pesadas sem necessidade;
- atualiza `README.md` e o vault Obsidian após cada alteração relevante;
- mantém `docs/estado-atual.md`, `docs/backlog-jogo.md`, `docs/roadmap-atual.md`, `docs/changelog-jogavel.md` e as notas técnicas coerentes;
- regista decisões de balanceamento e incompatibilidades de save.

### 7. Processo de execução

Trabalha por fatias verificáveis:

1. baseline e diagnóstico;
2. identidade/configuração das torres;
3. targeting e upgrade branches;
4. inimigos/counters/bosses;
5. ondas/economia/modos;
6. UI e apresentação;
7. saves, docs e limpeza;
8. playtest e regressão.

Depois de cada fatia:

- mostra os ficheiros alterados e a razão;
- executa build/testes;
- corrige regressões antes de avançar;
- não finjas que houve playtest visual se não abriste o executável;
- quando não conseguires verificar algo, cria uma checklist manual objetiva.

No final entrega:

- resumo do que mudou;
- lista de ficheiros;
- comandos executados e resultados;
- problemas ainda abertos;
- instruções de playtest;
- próximos três passos de maior impacto.

Começa agora pelo diagnóstico do repositório e não por uma longa explicação abstrata. Faz decisões concretas, implementa o máximo seguro e deixa o jogo sempre compilável.
