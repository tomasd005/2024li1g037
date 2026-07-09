# Extras implementados

Este documento resume as funcionalidades extra acrescentadas ao Immutable Towers.

Para o estado jogavel atual e correcoes recentes, ver [[estado-atual]].

## Novos projeteis

- Fogo: dano direto e dano continuo.
- Gelo: controlo forte, parando o inimigo temporariamente.
- Resina: abranda inimigos.
- Medo: faz o inimigo recuar durante a duracao do efeito.
- Veneno: causa dano continuo mais agressivo.
- Eletrico: atordoa por pouco tempo e funciona bem contra grupos por ter rajada maior.

## Sinergias

- Fogo + Resina prolonga o fogo.
- Gelo + Eletrico cria atordoamento eletrico adicional.
- Medo + Veneno aumenta o tempo de panico.
- Fogo + Eletrico causa dano extra imediato.
- Veneno + Resina causa dano extra.

## Terrenos especiais

- Asfalto: terreno caminhavel que aumenta a velocidade dos inimigos.
- O mapa avancado troca o caminho principal por asfalto, criando uma variante mais dificil.

## Inimigos com comportamento variado

As ondas agora geram inimigos por nivel:

- inimigos rapidos aparecem periodicamente
- inimigos brutos tem mais vida, dano e recompensa
- as ondas escalam vida, velocidade, dano e butim

## Melhorias de torres

O jogador pode selecionar uma torre colocada e carregar em `U` para gastar creditos. O upgrade melhora:

- dano
- alcance
- velocidade de disparo
- rajada, em torres mais fortes
- duracao do efeito do projetil

O preco do upgrade tambem escala com o poder atual da torre.

Melhorias recentes:

- preview de upgrade no painel lateral antes da compra
- feedback visual no momento do upgrade
- modelos das torres com mais detalhes visuais conforme o poder/upgrade

## Modos de jogo

- Historia: ondas equilibradas.
- Infinito: sobrevivencia com ondas geradas continuamente.
- Desafio: menos vida e ondas mais fortes.
- Boss: poucas ondas, mas inimigos duros.
- Sandbox: muitos creditos para testar torres e upgrades.

## Guardar e carregar

- `S`: guarda o jogo atual em `immutable-towers-save.txt`.
- `L`: carrega o ultimo jogo guardado.

## Progressao

- Perfil local com jogos, vitorias, derrotas e melhor pontuacao.
- Desbloqueio progressivo de torres.
- Gemas e baus.
- Fusao para a torre Tempestade.

## Editor de mapa

- `E` no menu abre o editor.
- Clicar numa celula alterna entre relva, terra, asfalto e agua.
- `ENTER` ou `ESC` volta ao menu.

## Obstaculos dinamicos

- `O` durante o jogo transforma a celula de caminho sob o rato em relva, funcionando como obstaculo simples.

## Bot / sugestao

- O painel lateral mostra uma sugestao automatica simples.
- `B` coloca automaticamente uma torre compravel numa celula valida perto do caminho.

## Interface e grafismo

- O mapa foi aumentado para reduzir margens pretas.
- A shop passou a funcionar numa sidebar lateral esquerda.
- Novos efeitos visuais indicam fogo, gelo, resina, medo, veneno e eletricidade.
- Os disparos agora mostram um impacto visual mais claro no alvo.
- A UI passou a bloquear corretamente o clique no mapa por baixo.
- O editor e os atalhos tornam as funcionalidades extra acessiveis sem menus complexos.
