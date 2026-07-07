module WaveSystem
  ( criaInimigo,
    criaOnda,
    ondasParaModo,
    onda01,
    onda02,
    onda03,
  )
where

import ImmutableTowers (ModoJogoEscolhido (..))
import LI12425

onda01, onda02, onda03 :: Onda
onda01 = criaOnda 1 4 0
onda02 = criaOnda 2 5 4
onda03 = criaOnda 3 7 8

criaOnda :: Int -> Int -> Tempo -> Onda
criaOnda nivel quantidade entrada =
  Onda
    { inimigosOnda = [criaInimigo nivel i | i <- [1 .. quantidade]],
      cicloOnda = max 0.9 (3.4 - fromIntegral nivel * 0.22),
      tempoOnda = 0,
      entradaOnda = entrada
    }

criaInimigo :: Int -> Int -> Inimigo
criaInimigo nivel indice =
  let escala = fromIntegral nivel
      rapido = indice `mod` 4 == 0
      bruto = indice `mod` 5 == 0
      vida = 60 + escala * 24 + if bruto then escala * 26 else 0
      velocidade = (if rapido then 2.4 else 1.25) + min 1.8 (escala * 0.08)
      ataque = 8 + escala * 1.6 + if bruto then 6 else 0
      butim = 14 + nivel * 4 + if bruto then 8 else 0
   in Inimigo {posicaoInimigo = (0, 2), direcaoInimigo = Este, vidaInimigo = vida, velocidadeInimigo = velocidade, ataqueInimigo = ataque, butimInimigo = butim, projeteisInimigo = []}

ondasParaModo :: ModoJogoEscolhido -> [Onda]
ondasParaModo modoAtual = case modoAtual of
  ModoHistoria -> [criaOnda 1 4 0, criaOnda 2 5 4, criaOnda 3 7 8, criaOnda 4 8 12]
  ModoInfinito -> [criaOnda 1 5 0]
  ModoDesafio -> [criaOnda 3 6 0, criaOnda 5 8 3, criaOnda 7 10 6]
  ModoBoss -> [criaOnda 2 5 0, criaOnda 6 2 5, criaOnda 10 1 9]
  ModoSandbox -> [criaOnda 1 4 0, criaOnda 2 5 5]
