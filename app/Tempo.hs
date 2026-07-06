module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3

reageTempo :: Tempo -> ImmutableTowers -> IO ImmutableTowers
reageTempo segundos (ImmutableTowers jogo imgs EmJogo tempoAtual torreSel rato) =
  let segundosFloat = realToFrac segundos

      -- Atualiza o jogo inteiro com o tempo decorrido
      jogoAtualizado = atualizaJogo segundosFloat jogo

      -- Tempo acumulado
      tempoNovo = tempoAtual + segundos
   in return $ ImmutableTowers jogoAtualizado imgs EmJogo tempoNovo torreSel rato
-- Outros modos não alteram o estado
reageTempo _ estado = return estado
