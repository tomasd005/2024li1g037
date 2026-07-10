# Immutable Towers - Backlog

Tags: #backlog #prioridade

Backlog vivo do Immutable Towers.

Regra: esta nota deve conter apenas trabalho em aberto. O que ja ficou feito sai daqui e passa para [[estado-atual]] ou [[extras-implementados]].

Nota: este backlog foi limpo para deixar aqui apenas melhorias e validacoes que ainda faltam fechar. Sidebar, preview de upgrade, bloqueio de clique no mapa e feedback visual base ja estao implementados.

Relacionadas:

- [[HOME]]
- [[estado-atual]]
- [[feedback-jogo-lei120arch-2026-07-08]]
- [[extras-implementados]]

## Alta prioridade

- Validar em playtest se a nova sidebar da loja ficou ergonomica em varias resolucoes.
- Unificar ainda mais as hitboxes de render e input para evitar discrepancias entre o que se ve e o que se pode clicar.
- Guardar a identidade da torre de forma explicita no modelo, sem depender apenas de inferencias por projetil ou estatisticas.
- Continuar a evoluir o modelo visual das torres por tier, para alem da progressao por upgrade.
- Refinar ainda mais o feedback de hit/dano, apesar de ja existirem numeros e impacto visual.

## Media prioridade

- Adicionar mais variacoes visuais e animacoes de upgrade.
- Limpar textos antigos com encoding estranho e terminar a uniformizacao da tipografia in-game.
- Tornar as opcoes de video realmente configuraveis, em vez de apenas informativas.

## Baixa prioridade

- Limpar warnings antigos dos testes.

## Bugs abertos

### Input/UI

- Verificar em playtest se nao restam zonas mortas ou ambiguas no overlay da UI.
- Rever a correspondencia entre layout desenhado e hitboxes reais em sidebar, HUD e paineis contextuais.

### Gameplay

- Confirmar em jogo real se nao ha outros efeitos que deixem inimigos presos fora do caso ja corrigido.

## Melhorias de design

### Upgrade UX

- Evoluir o efeito atual de upgrade para algo mais rico, com brilho, particulas ou model swap.
- Tornar ainda mais visual a comparacao no hover do botao de upgrade.
- Adicionar uma evolucao visual mais forte nos upgrades altos, para que a diferenca de poder seja imediatamente percetivel.

### Torres

- Criar modelos ainda mais distintos por tier, com silhuetas, cores e detalhes que se reconhecam rapido.
- Separar melhor a identidade base da torre da sua fase de upgrade, para facilitar expansao futura de arsenal, skins ou fusoes.

### Loja

- Melhorar a sidebar com categorias, scroll ou agrupamento se o numero de torres aumentar.
- Garantir que a sidebar continua legivel e com boa navegacao quando houver muitas torres.
- Validar se a ergonomia da loja se mantem boa em mais rondas de playtest, com diferentes resolucoes e tamanhos de janela.

## Sugestao de ordem de ataque

1. Validar e refinar a nova sidebar da loja.
2. Unificar hitboxes e zonas interativas com o render real.
3. Guardar identidade explicita das torres no modelo.
4. Melhorar modelos/animacoes das torres.
5. Reforcar feedback visual de combate.
6. Expandir e manter a documentacao tecnica por sistema.
