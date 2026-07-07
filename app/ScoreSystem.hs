module ScoreSystem
  ( pontuacaoAtual,
  )
where

import ImmutableTowers
import LI12425

pontuacaoAtual :: ImmutableTowers -> Int
pontuacaoAtual e =
  let base = baseJogo (jogo e)
      vida = floor (vidaBase base) :: Int
      creditos = creditosBase base
      torres = length (torresJogo (jogo e))
      bonusModo = case modoJogoEscolhido e of
        ModoHistoria -> 0
        ModoInfinito -> ondasSobrevividas e * 120
        ModoDesafio -> 350
        ModoBoss -> 500
        ModoSandbox -> -150
   in max 0 (floor (tempo e * 8) + vida * 15 + creditos + torres * 60 + bonusModo)
