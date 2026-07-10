# Immutable Towers - Feedback LEI 120 Arch - 2026-07-08

Tags: #feedback #playtest

Feedback informal recolhido durante uma sessao de teste ao jogo.

Relacionadas:

- [[HOME]]
- [[estado-atual]]
- [[backlog-jogo]]
- [[extras-implementados]]
- [[etapa5-roadmap-features]]

## Problemas e sugestoes identificados

### Efeitos e comportamento de torres

- [resolvido] Havia casos em que os inimigos levavam um tiro e ficavam parados para sempre.
- [resolvido] O efeito de gelo/controlo estava a persistir mais do que devia em alguns casos.
- [resolvido] Dar upgrade numa torre agora tem feedback visual proprio, para alem da notificacao textual.
- [parcial] O upgrade agora tambem se reflete melhor no visual da torre, e a leitura por tier melhorou, mas ainda ha margem para mais identidade.

### Informacao de upgrade

- [resolvido] Ao preparar um upgrade, passou a ser claro o que vai mudar antes da compra.
- [parcial] O painel da torre ja mostra preview mais completo, mas hover dedicado no botao pode ainda ser refinado.
- Idealmente o jogador devia ver pelo menos:
  - dano atual -> dano novo
  - alcance atual -> alcance novo
  - cadencia/ciclo atual -> novo
  - custo do upgrade

### Loja e colocacao de torres

- [resolvido] Ao selecionar uma torre e depois tentar trocar para outra, ja nao deve construir no terreno por tras da UI.
- [resolvido] O conflito principal entre clique na UI e clique no mapa foi tratado.
- [resolvido] A loja deixou de ficar no rodape sobre o mapa e passou para sidebar lateral.

### Sugestoes de layout

- [resolvido] A loja passou para uma sidebar lateral em vez de ficar sobre o mapa.
- [resolvido] A area dos botoes/loja passou a bloquear a interacao com o mapa por baixo.
- Sidebar continua a parecer a opcao mais limpa para evitar spawn acidental de torres.

### Processo e organizacao

- O vault de Obsidian passou a ser usado para documentacao viva.
- Continua a fazer sentido manter backlog, estado atual e feedback separados.

## Interpretacao tecnica

Os problemas acima apontavam sobretudo para quatro zonas:

1. Separacao entre input de UI e input de mapa.
2. Persistencia/limpeza de estados temporarios quando se troca a torre selecionada.
3. Feedback visual insuficiente em upgrades.
4. Falta de documentacao centralizada para facilitar testes e sugestoes.

## Candidatos a prioridade alta

- [feito] Corrigir o bug de inimigos presos apos certos tiros.
- [feito] Garantir que clicar na UI nunca coloca torres no mapa.
- [feito] Mostrar preview claro do upgrade antes da compra.
- [feito] Reorganizar a loja para um layout menos sobreposto ao mapa.
