module UIState
  ( WaveSummary (..),
    waveSummary,
  )
where

import ImmutableTowers
import qualified Data.IntMap.Strict as IntMap
import EnemySystem
import LI12425

data WaveSummary = WaveSummary
  { ondaAtualUI :: !Int,
    ondasTotaisUI :: !Int,
    ondasRestantesUI :: !Int,
    inimigosRestantesUI :: !Int,
    proximaOndaUI :: !(Maybe Int),
    composicaoProximaUI :: [(EnemyClass, Int)]
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
      ondasComInimigos = filter (not . null . inimigosOnda) ondasRestantes
      (ondaPreview, avancaPreview) = case ondasComInimigos of
        [] -> (Nothing, False)
        ondaAtualPendente : proximaPendente : _
          | not (null ativos) && entradaOnda ondaAtualPendente <= 0 -> (Just proximaPendente, True)
        onda : _ -> (Just onda, False)
      proxima = case ondaPreview of
        Nothing -> Nothing
        Just _ -> Just (max 1 (min (max 1 totalModo) (ondaAtual + if avancaPreview then 1 else 0)))
      composicao = maybe [] composicaoOnda ondaPreview
   in WaveSummary
        { ondaAtualUI = ondaAtual,
          ondasTotaisUI = totalModo,
          ondasRestantesUI = restantes,
          inimigosRestantesUI = inimigosRestantes,
          proximaOndaUI = proxima,
          composicaoProximaUI = composicao
        }

composicaoOnda :: Onda -> [(EnemyClass, Int)]
composicaoOnda onda =
  [ (enemyClass, quantidade)
  | enemyClass <- [minBound .. maxBound],
    let quantidade = IntMap.findWithDefault 0 (fromEnum enemyClass) contagens,
    quantidade > 0
  ]
  where
    contagens = foldl' conta IntMap.empty (inimigosOnda onda)
    conta acumulador inimigo =
      IntMap.insertWith (+) (fromEnum (enemyClassOf inimigo)) 1 acumulador
