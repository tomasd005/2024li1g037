module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import Tarefa1
import LI12425

testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ "validaJogo (válido)" ~: True ~=? validaJogo jogo1,
        "validaJogo (portal inválido)" ~: False ~=? validaJogo jogoInvalidoPortal,
        "validaPortais (válido)" ~: True ~=? validaPortais jogo1,
        "validaPortais (portal fora do mapa)" ~: False ~=? validaPortais jogoInvalidoPortal,
        "posicaoPortalValida (válido)" ~: True ~=? posicaoPortalValida (0, 0) mapa1,
        "posicaoPortalValida (sobre água)" ~: False ~=? posicaoPortalValida (3, 0) mapa1,
        "posicaoPortalValida (fora do mapa)" ~: False ~=? posicaoPortalValida (-1, -1) mapa1,
        "validaOndaPortal (sem inimigos)" ~: True ~=? validaOndaPortal portal1,
        "validaOndaPortal (com inimigos)" ~: False ~=? validaOndaPortal portalComInimigos,
        "ondaSemInimigos (vazia)" ~: True ~=? ondaSemInimigos (Onda [] 1 1 1),
        "ondaSemInimigos (não vazia)" ~: False ~=? ondaSemInimigos (Onda [inimigo1] 1 1 1),
        "minimoPortal (com portais)" ~: True ~=? minimoPortal [portal1],
        "minimoPortal (sem portais)" ~: False ~=? minimoPortal [],
        "terrenoPorPosicao (terra)" ~: Just Terra ~=? terrenoPorPosicao (0, 0) mapa1,
        "terrenoPorPosicao (fora do mapa)" ~: Nothing ~=? terrenoPorPosicao (-1, -1) mapa1,
        "posicionadoEmTerra (válido)" ~: True ~=? posicionadoEmTerra mapa1 [portal1],
        "posicionadoEmTerra (sobre água)" ~: False ~=? posicionadoEmTerra mapa1 [portalAgua],
        "posicionadoEmRelvaTorre (válido)" ~: True ~=? posicionadoEmRelvaTorre mapa1 [torre1],
        "posicionadoEmRelvaTorre (sobre terra)" ~: False ~=? posicionadoEmRelvaTorre mapa1 [torreTerra],
        "naoSobrepostosTorreBase (válido)" ~: True ~=? naoSobrepostosTorreBase [] base1 [torre1] [portal1] mapa1,
        "verificaPosicaoTorreEmPortal (válido)" ~: True ~=? verificaPosicaoTorreEmPortal mapa1 [torre1] [portal1],
        "verificaPosicaoTorreEmPortal (torre sobre portal)" ~: False ~=? verificaPosicaoTorreEmPortal mapa1 [torre1 { posicaoTorre = (0, 0) }] [portal1],
        "verificaPosicaoBaseEmPortal (válido)" ~: True ~=? verificaPosicaoBaseEmPortal mapa1 base1 [portal1],
        "verificaPosicaoBaseEmPortal (base sobre portal)" ~: False ~=? verificaPosicaoBaseEmPortal mapa1 base1 { posicaoBase = (0, 0) } [portal1],
        "maximoOndaPorPortal (válido)" ~: True ~=? maximoOndaPorPortal [portal1],
        "maximoOndaPorPortal (múltiplas ondas)" ~: False ~=? maximoOndaPorPortal [portalMultiplasOndas],
        "caminhoPortalBase (caminho existente)" ~: True ~=? caminhoPortalBase mapa1 [(0, 0)] (1, 1),
        "caminhoPortalBase (caminho inexistente)" ~: False ~=? caminhoPortalBase mapaBloqueado [(0, 0)] (1, 1),
        "buscaCaminho (caminho existente)" ~: True ~=? buscaCaminho mapa1 (0, 0) (1, 1) [],
        "buscaCaminho (posição inválida)" ~: False ~=? buscaCaminho mapa1 (-1, -1) (1, 1) [],
        "buscaCaminho (sem caminho)" ~: False ~=? buscaCaminho mapaBloqueado (0, 0) (5, 5) [],
        "buscaCaminho (todos vizinhos visitados)" ~: False ~=? buscaCaminho mapa1 (1, 1) (0, 0) [(0,1), (1,0), (2,1), (1,2)],
        "buscaCaminho (destino em terreno inválido)" ~: False ~=? buscaCaminho mapa1 (0, 0) (5, 0) [],
        "adjacentes (posição central)" ~: [(0.0,1.0),(2.0,1.0),(1.0,0.0),(1.0,2.0)] ~=? adjacentes (1.0, 1.0),
        "posicaoValida (válida)" ~: True ~=? posicaoValida mapa1 (1.5, 2.5),
        "posicaoValida (inválida)" ~: False ~=? posicaoValida mapa1 (-1, -1),
        "validaInimigos (válido)" ~: True ~=? validaInimigos jogo1,
        "validaInimigos (vida negativa)" ~: False ~=? validaInimigos jogoInimigoVidaNegativa,
        "validaTorres (válido)" ~: True ~=? validaTorres jogo1,
        "validaTorres (alcance negativo)" ~: False ~=? validaTorres jogoTorreAlcanceNegativo,
        "todasEmRelva (válido)" ~: True ~=? todasEmRelva (mapaJogo jogo1) (torresJogo jogo1),
        "alcancesPositivos (válido)" ~: True ~=? alcancesPositivos (torresJogo jogo1),
        "alcancesPositivos (negativo)" ~: False ~=? alcancesPositivos [torreAlcanceNegativo],
        "rajadasPositivas (válido)" ~: True ~=? rajadasPositivas (torresJogo jogo1),
        "rajadasPositivas (negativa)" ~: False ~=? rajadasPositivas [torreRajadaNegativa],
        "ciclosNaoNegativos (válido)" ~: True ~=? ciclosNaoNegativos (torresJogo jogo1),
        "ciclosNaoNegativos (negativo)" ~: False ~=? ciclosNaoNegativos [torreCicloNegativo],
        "naoSobrepostas (válido)" ~: True ~=? naoSobrepostas (torresJogo jogo1),
        "naoSobrepostas (sobrepostas)" ~: False ~=? naoSobrepostas [torre1, torre1],
        "validaBase (válido)" ~: True ~=? validaBase jogo1,
        "validaBase (sobreposição com torre)" ~: False ~=? validaBase jogoBaseSobreTorre
      ]

-- Jogos e entidades auxiliares
jogoInvalidoPortal = jogo1 { portaisJogo = [portalAgua] }
portalAgua = Portal { posicaoPortal = (3, 0), ondasPortal = [] }
portalComInimigos = Portal { posicaoPortal = (0, 0), ondasPortal = [Onda [inimigo1] 1 1 1] }
portalMultiplasOndas = Portal { posicaoPortal = (0, 0), ondasPortal = [Onda [] 1 1 1, Onda [] 1 1 1] }
mapaBloqueado = replicate 6 [Agua, Agua, Agua, Agua, Agua, Agua]
jogoInimigoVidaNegativa = jogo1 { inimigosJogo = [inimigo1 { vidaInimigo = -10 }] }
jogoTorreAlcanceNegativo = jogo1 { torresJogo = [torre1 { alcanceTorre = -5 }] }
torreAlcanceNegativo = torre1 { alcanceTorre = -5 }
torreRajadaNegativa = torre1 { rajadaTorre = -1 }
torreCicloNegativo = torre1 { cicloTorre = -1 }
jogoBaseSobreTorre = jogo1 { baseJogo = base1 { posicaoBase = posicaoTorre torre1 } }
torreTerra = torre1 { posicaoTorre = (0, 0) }
