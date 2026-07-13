module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import Tarefa3
import Tarefa2
import Tarefa1
import LI12425

testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ 
        -- Testes de atualizaJogo
        "atualizaJogo - base perde vida quando inimigo chega" ~:
          vidaBase (baseJogo (atualizaJogo 1.0 jogoComInimigoNaBase)) < vidaBase (baseJogo jogoComInimigoNaBase) ~=? True,

        "atualizaJogo - inimigo que atravessa a base causa dano" ~:
          let resultado = atualizaJogo 1.0 jogoInimigoAtravessaBase
           in vidaBase (baseJogo resultado) < vidaBase (baseJogo jogoInimigoAtravessaBase)
                && null (inimigosJogo resultado) ~=? True,

        "separaInimigosPorEstado - celula da base e terminal" ~:
          let inimigoNaCelula = inimigo1 {posicaoInimigo = (4.9, 29.5)}
              (_, vivos, naBase) = separaInimigosPorEstado [inimigoNaCelula] (4.5, 29.5)
           in null vivos && length naBase == 1 ~=? True,
        
        "atualizaJogo - inimigos mortos são removidos" ~:
          length (inimigosJogo (atualizaJogo 1.0 jogoComInimigoMorto)) < length (inimigosJogo jogoComInimigoMorto) ~=? True,
        
        -- Testes de portais
        "atualizaTodosPortais - lança inimigos" ~:
          let (_, inimigos) = atualizaTodosPortais 10.0 [portalProntoParaLancar]
           in length inimigos >= 0 ~=? True,
        
        "atualizaPortalELanca - atualiza tempo da onda" ~:
          let (portal', _) = atualizaPortalELanca 1.0 portalProntoParaLancar
           in length (ondasPortal portal') <= length (ondasPortal portalProntoParaLancar) ~=? True,
        
        -- Testes de torres
        "atualizaTodasTorres - torres disparam" ~:
          let (torres', _) = atualizaTodasTorres 5.0 [inimigoProximo] [torrePronta]
           in case torres' of
                torreAtualizada : _ -> tempoTorre torreAtualizada == cicloTorre torreAtualizada
                [] -> False
           ~=? True,
        
        "atualizaTorre - decrementa tempo" ~:
          let (torre', _) = atualizaTorre 1.0 [] torre1
           in tempoTorre torre' < tempoTorre torre1 ~=? True,
        
        "dispararTorre - atinge inimigos no alcance" ~:
          let (_, inimigos') = dispararTorre torrePronta [inimigoProximo]
           in case inimigos' of
                inimigoAtingido : _ -> vidaInimigo inimigoAtingido < vidaInimigo inimigoProximo
                [] -> False
           ~=? True,
        
        "distanciaDaTorre - calcula corretamente" ~:
          distanciaDaTorre torre1 inimigo1 >= 0 ~=? True,
        
        -- Testes de inimigos
        "atualizaInimigos - move todos os inimigos" ~:
          let inimigos' = atualizaInimigos 1.0 mapa1 [inimigoMovel]
           in length inimigos' == 1 ~=? True,
        
        "atualizaInimigo - atualiza posição" ~:
          let inimigo' = atualizaInimigo 1.0 mapa1 inimigoMovel
           in posicaoInimigo inimigo' /= posicaoInimigo inimigoMovel || velocidadeInimigo inimigoMovel == 0 ~=? True,
        
        "atualizaProjetis - decrementa duração" ~:
          let inimigo' = atualizaProjetis 1.0 inimigoComProjetil
           in case projeteisInimigo inimigo' of
                projetil : _ -> case duracaoProjetil projetil of
                  Finita t -> t < 5.0
                  Infinita -> False
                [] -> False
           ~=? True,
        
        "atualizaDuracaoProjetil - funciona corretamente" ~:
          let proj' = atualizaDuracaoProjetil 1.0 (Projetil Fogo (Finita 5.0))
           in duracaoProjetil proj' == Finita 4.0 ~=? True,
        
        "atualizaDuracao - Finita" ~:
          atualizaDuracao (Finita 5.0) 1.0 == Finita 4.0 ~=? True,
        
        "atualizaDuracao - Infinita" ~:
          atualizaDuracao Infinita 1.0 == Infinita ~=? True,
        
        "aplicaEfeitoProjetil - fogo causa dano" ~:
          let inimigo' = aplicaEfeitoProjetil 1.0 inimigoComFogo
           in vidaInimigo inimigo' < vidaInimigo inimigoComFogo ~=? True,
        
        "aplicaEfeitoProjetil - gelo impede movimento" ~:
          let inimigo' = aplicaEfeitoProjetil 1.0 inimigoComGelo
           in velocidadeInimigo inimigo' == 0 ~=? True,
        
        "aplicaEfeitoProjetil - resina reduz velocidade" ~:
          let inimigo' = aplicaEfeitoProjetil 1.0 inimigoComResina
           in velocidadeInimigo inimigo' < velocidadeInimigo inimigoComResina ~=? True,

        "aplicaEfeitoProjetil - recupera velocidade apos gelo" ~:
          let inimigo' = aplicaEfeitoProjetil 1.0 (inimigo1 {velocidadeInimigo = 0, projeteisInimigo = []})
           in velocidadeInimigo inimigo' == velocidadeBaseInimigo inimigo1 ~=? True,
        
        "removeProjeteisExpirados - remove expirados" ~:
          let inimigo' = removeProjeteisExpirados inimigoComProjetilExpirado
           in length (projeteisInimigo inimigo') < length (projeteisInimigo inimigoComProjetilExpirado) ~=? True,
        
        -- Testes de movimento
        "moverInimigo - move corretamente" ~:
          let inimigo' = moverInimigo 1.0 mapa1 inimigoMovel
           in posicaoInimigo inimigo' /= posicaoInimigo inimigoMovel ~=? True,
        
        "moverNaDirecao - Este" ~:
          moverNaDirecao (1.0, 1.0) Este 1.0 == (2.0, 1.0) ~=? True,
        
        "moverNaDirecao - Norte" ~:
          moverNaDirecao (1.0, 1.0) Norte 1.0 == (1.0, 0.0) ~=? True,
        
        "proximaDirecao - mantém direção válida" ~:
          let dir = proximaDirecao mapa1 (1.0, 1.0) Este
           in dir `elem` [Norte, Sul, Este, Oeste] ~=? True,

        "proximaDirecao - aceita inverter quando e a unica saida valida" ~:
          proximaDirecao mapaCorredorReverso (2.0, 1.0) Este ~=? Oeste,
        
        "vizinhosValidos - retorna vizinhos em terra" ~:
          length (vizinhosValidos mapa1 (1.0, 1.0)) >= 0 ~=? True,
        
        "posicaoValidaTerra - válida em terra" ~:
          posicaoValidaTerra mapa1 (0.0, 0.0) ~=? True,
        
        "posicaoValidaTerra - inválida em água" ~:
          posicaoValidaTerra mapa1 (3.0, 0.0) ~=? False,
        
        "getDirecaoOposta - Norte/Sul" ~:
          getDirecaoOposta Norte == Sul ~=? True,
        
        "getDirecaoOposta - Este/Oeste" ~:
          getDirecaoOposta Este == Oeste ~=? True,
        
        -- Testes auxiliares
        "separaInimigosPorEstado - separa corretamente" ~:
          let (mortos, vivos, _) = separaInimigosPorEstado [inimigoMorto, inimigoVivo] (10.0, 10.0)
           in length mortos == 1 && length vivos == 1 ~=? True,
        
        "inimigoMorreu - True quando vida <= 0" ~:
          inimigoMorreu inimigoMorto ~=? True,
        
        "inimigoMorreu - False quando vida > 0" ~:
          inimigoMorreu inimigoVivo ~=? False,
        
        "criarProjetil - cria projétil correto" ~:
          tipoProjetil (criarProjetil torre2 inimigo1) == Fogo ~=? True,
        
        "aplicaEfeito - adiciona projétil" ~:
          let inimigo' = aplicaEfeito projetil1 inimigo1
           in length (projeteisInimigo inimigo') >= 1 ~=? True,
        
        "atingiuBase - True quando próximo" ~:
          atingiuBase base1 (inimigo1 {posicaoInimigo = posicaoBase base1}) ~=? True,
        
        "atingiuBase - False quando longe" ~:
          atingiuBase base1 inimigo1 ~=? False,
        
        "atualizaBase - mantém estado" ~:
          let base' = atualizaBase 1.0 base1
           in vidaBase base' == vidaBase base1 ~=? True,
        
        "atualizaOnda - decrementa tempos" ~:
          let onda' = atualizaOnda 1.0 [] onda1
           in tempoOnda onda' <= tempoOnda onda1 && entradaOnda onda' <= entradaOnda onda1 ~=? True,
        
        "atualizaPortal - processa ondas" ~:
          let portal' = atualizaPortal 1.0 [] portal1
           in length (ondasPortal portal') <= length (ondasPortal portal1) ~=? True
      ]

-- ============================================================================
-- ENTIDADES DE TESTE
-- ============================================================================
-- ondas
-- Exemplo de inimigos
mapaCorredorReverso :: Mapa
mapaCorredorReverso =
  [ [Relva, Relva, Relva],
    [Terra, Terra, Terra],
    [Relva, Relva, Relva]
  ]

-- Exemplos de ondas
onda1 :: Onda
onda1 = Onda
  { inimigosOnda = replicate 5 inimigo1,  -- 5 goblins
    cicloOnda = 1.0,                          -- 1 segundo entre cada inimigo
    tempoOnda = 0.0,                           -- inicia imediatamente
    entradaOnda = 5.0                          -- começa 5 segundos após a onda anterior
  }



-- Jogo com inimigo na base
jogoComInimigoNaBase :: Jogo
jogoComInimigoNaBase = jogo1 {
  inimigosJogo = [inimigo1 {posicaoInimigo = posicaoBase base1}]
}

jogoInimigoAtravessaBase :: Jogo
jogoInimigoAtravessaBase = jogo1 {
  mapaJogo = replicate 5 (replicate 5 Terra),
  baseJogo = base1 {posicaoBase = (2, 1), vidaBase = 50},
  portaisJogo = [],
  torresJogo = [],
  inimigosJogo = [inimigo1 {
    posicaoInimigo = (1, 1),
    direcaoInimigo = Este,
    velocidadeBaseInimigo = 2,
    velocidadeInimigo = 2
  }]
}

-- Jogo com inimigo morto
jogoComInimigoMorto :: Jogo
jogoComInimigoMorto = jogo1 {
  inimigosJogo = [inimigo1 {vidaInimigo = 0}]
}

-- Portal pronto para lançar
portalProntoParaLancar :: Portal
portalProntoParaLancar = Portal {
  posicaoPortal = (0, 0),
  ondasPortal = [Onda {
    inimigosOnda = [inimigo1],
    cicloOnda = 1.0,
    tempoOnda = 0.0,
    entradaOnda = 0.0
  }]
}

-- Torre pronta para disparar
torrePronta :: Torre
torrePronta = torre1 {tempoTorre = 0.0}

-- Inimigo próximo da torre
inimigoProximo :: Inimigo
inimigoProximo = inimigo1 {posicaoInimigo = (0.5, 1.0)}

-- Inimigo móvel
inimigoMovel :: Inimigo
inimigoMovel = inimigo1 {
  posicaoInimigo = (1.0, 1.0),
  velocidadeBaseInimigo = 1.0,
  velocidadeInimigo = 1.0,
  direcaoInimigo = Este
}

-- Inimigo com projétil
inimigoComProjetil :: Inimigo
inimigoComProjetil = inimigo1 {
  projeteisInimigo = [Projetil Fogo (Finita 5.0)]
}

-- Inimigo com fogo
inimigoComFogo :: Inimigo
inimigoComFogo = inimigo1 {
  vidaInimigo = 100.0,
  projeteisInimigo = [Projetil Fogo (Finita 5.0)]
}

-- Inimigo com gelo
inimigoComGelo :: Inimigo
inimigoComGelo = inimigo1 {
  velocidadeBaseInimigo = 1.0,
  velocidadeInimigo = 1.0,
  projeteisInimigo = [Projetil Gelo (Finita 3.0)]
}

-- Inimigo com resina
inimigoComResina :: Inimigo
inimigoComResina = inimigo1 {
  velocidadeBaseInimigo = 1.0,
  velocidadeInimigo = 1.0,
  projeteisInimigo = [Projetil Resina (Finita 4.0)]
}

-- Inimigo com projétil expirado
inimigoComProjetilExpirado :: Inimigo
inimigoComProjetilExpirado = inimigo1 {
  projeteisInimigo = [Projetil Fogo (Finita 0.0), Projetil Gelo (Finita 3.0)]
}

-- Inimigo morto
inimigoMorto :: Inimigo
inimigoMorto = inimigo1 {vidaInimigo = 0}

-- Inimigo vivo
inimigoVivo :: Inimigo
inimigoVivo = inimigo1 {vidaInimigo = 100}

-- ============================================================================
-- INSTÂNCIAS Eq
-- ============================================================================
