# Immutable Towers - Roadmap Etapa 5

Tags: #roadmap #arquivo

Nota: esta pagina serve como direcao de design e roadmap. Para o estado real atual do projeto, ver [[estado-atual]].

## Referencias de design usadas

Tower defense modernos tendem a assentar em quatro pilares:

1. variedade de torres
2. progressao e upgrades
3. modos de jogo
4. metagame leve

## Ordem proposta de implementacao

### 5.1 Ajuste visual imediato

- aumentar o zoom util do mapa
- manter a conversao de cliques sincronizada com a escala visual
- reduzir sobreposicao dos paineis no campo jogavel

### 5.2 Conta / utilizador local

- `PerfilJogador` com nome, jogos, vitorias, derrotas e melhor score
- ecra de perfil
- persistencia local

### 5.3 Leaderboard local

- registar pontuacao por partida
- guardar nome, modo e desempenho
- ecra de leaderboard local

### 5.4 Loja expandida

Cada torre deve ter:

- tipo
- nome
- modelo visual
- custo base
- nivel
- caminho de upgrades
- preco do proximo upgrade

### 5.5 Upgrades

Escalamento sugerido:

- preco cresce com o poder atual
- dano aumenta de forma clara por nivel
- alcance aumenta menos do que dano
- ciclo melhora com moderacao
- utilidade especial melhora tambem

### 5.6 Modos de jogo

- Historia
- Infinito
- Desafio
- Boss
- Sandbox
