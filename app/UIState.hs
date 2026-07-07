module UIState
  ( WaveSummary (..),
    waveSummary,
  )
where

import ImmutableTowers
import LI12425

data WaveSummary = WaveSummary
  { ondaAtualUI :: !Int,
    ondasTotaisUI :: !Int,
    ondasRestantesUI :: !Int,
    inimigosRestantesUI :: !Int,
    proximaOndaUI :: !(Maybe Int)
  }

waveSummary :: ImmutableTowers -> WaveSummary
waveSummary estado =
  let ondasRestantes = concatMap ondasPortal (portaisJogo (jogo estado))
      totalModo = totalOndasPartida estado
      restantes = length ondasRestantes
      ativos = inimigosJogo (jogo estado)
      inimigosFila = sum (map (length . inimigosOnda) ondasRestantes)
      inimigosRestantes = length ativos + inimigosFila
      ondaAtual
        | modoJogoEscolhido estado == ModoInfinito = max 1 (ondasSobrevividas estado + 1)
        | totalModo <= 0 = 0
        | restantes <= 0 = totalModo
        | otherwise = min totalModo (totalModo - restantes + 1)
      proxima
        | restantes <= 1 = Nothing
        | otherwise = Just (min totalModo (ondaAtual + 1))
   in WaveSummary
        { ondaAtualUI = ondaAtual,
          ondasTotaisUI = totalModo,
          ondasRestantesUI = restantes,
          inimigosRestantesUI = inimigosRestantes,
          proximaOndaUI = proxima
        }
