# Etapa 5 — Sistemas de progressão e modos

## Referências de design usadas

Tower defense modernos tendem a assentar em quatro pilares:

1. **Variedade de torres** — cada torre resolve um problema diferente: dano direto, área, lentidão, dano contínuo, suporte ou economia.
2. **Progressão/upgrade** — upgrades aumentam utilidade e preço, criando decisões de investimento em vez de apenas comprar sempre a mesma torre.
3. **Modos de jogo** — campanha/história para aprendizagem e progressão, infinito para score/leaderboard, desafios com restrições para rejogabilidade.
4. **Metagame leve** — perfil de jogador, pontuação, recordes e leaderboard local dão contexto ao desempenho.

## Ordem proposta de implementação

### 5.1 Ajuste visual imediato

- Aumentar o zoom do mapa para reduzir as bordas pretas laterais.
- Manter a conversão de cliques sincronizada com a escala visual.
- Encolher/deslocar o painel lateral para não tapar tanto o mapa.

### 5.2 Conta / utilizador local

Implementar um sistema local simples, sem rede:

- `PerfilJogador` com nome, jogos jogados, vitórias, derrotas e melhor pontuação.
- Ecrã de perfil no menu.
- Nome editável por teclado.
- Persistência futura em ficheiro local simples.

### 5.3 Leaderboard local

- Registar pontuação por partida.
- Guardar nome do jogador, modo, ondas sobrevividas, inimigos derrotados, créditos restantes e tempo.
- Ecrã de leaderboard ordenado por pontuação.

### 5.4 Loja expandida

Adicionar tipos de torre com identidade própria:

- **Resina**: controlo/lentidão, baixa cadência, utilidade alta.
- **Gelo**: slow em área ou duração maior.
- **Fogo**: dano por segundo / dano contínuo.
- **Canhão**: dano em área, lento e caro.
- **Arqueiro/Balista**: barato, longo alcance, dano moderado.
- **Suporte**: aumenta alcance/dano de torres próximas.

Cada torre deve ter:

- tipo;
- nome;
- modelo visual;
- custo base;
- nível;
- caminho de upgrades;
- preço do próximo upgrade.

### 5.5 Upgrades

Escalamento sugerido:

- Preço: `precoBase * nivel * 2`.
- Dano: aumenta 25–40% por nível.
- Alcance: aumenta pouco, 10–15%.
- Cadência/ciclo: reduz ligeiramente.
- Utilidade especial: melhora duração/área/efeito secundário.

### 5.6 Modos de jogo

- **História**: mapas/ondas predefinidos, dificuldade gradual.
- **Infinito**: ondas geradas progressivamente, foco em score.
- **Desafio**: restrições como só uma classe de torre ou créditos reduzidos.
- **Sandbox/Treino**: créditos altos para experimentar torres/upgrades.

## Primeira implementação recomendada

Começar por 5.1 + estrutura base de 5.2/5.3:

1. Ajustar zoom.
2. Criar tipos `PerfilJogador`, `Pontuacao`, `ModoJogoEscolhido` no estado da app.
3. Adicionar opções de menu para Perfil, Leaderboard e Modos.
4. Só depois ligar pontuação real ao fim da partida.
