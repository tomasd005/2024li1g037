# Sistema Torres

Relacionadas:

- [[HOME]]
- [[estado-atual]]
- [[extras-implementados]]

## Responsabilidades

- definicao de torres base
- raridade/tier
- custo de compra
- upgrades
- valor de venda
- leitura visual da progressao

## Modulos principais

- `app/TowerSystem.hs`
- `app/Desenhar.hs`
- `lib/Tarefa2.hs`
- `lib/Tarefa3.hs`

## Estado atual

- varias torres com identidades diferentes por projetil
- upgrades escalam dano, alcance, ciclo, rajada e duracao do efeito
- o modelo visual da torre muda:
  - por raridade aproximada
  - por nivel de poder/upgrade

## Decisoes atuais

- a classificacao visual por tier usa `towerSpecAproximada`
- isto evita reestruturar toda a `Torre` so para guardar `TowerId`
- o modelo atual privilegia performance e legibilidade em vez de detalhe pesado

## Proximos melhoramentos

- guardar identidade da torre de forma explicita no modelo
- criar modelos ainda mais distintos por tier
- adicionar evolucao visual mais forte nos upgrades altos
