module GameFactory
  ( baseParaModo,
    prepararPartida,
    modoDesbloqueado,
    nivelMinimoModo,
  )
where

import ImmutableTowers (ModoJogoEscolhido (..))
import LI12425
import MapData
import MetaTypes
import ProgressionSystem
import TowerSystem
import WaveSystem

baseParaModo :: ModoJogoEscolhido -> MapId -> Base
baseParaModo modoAtual mapaId =
  (basePorMapa mapaId)
    { creditosBase = case modoAtual of
        ModoSandbox -> 999
        ModoDesafio -> 125
        ModoBoss -> 180
        ModoInfinito -> 165
        ModoHistoria -> 150,
      vidaBase = case modoAtual of
        ModoDesafio -> 60
        ModoBoss -> 110
        _ -> 80
    }

prepararPartida :: ModoJogoEscolhido -> MetaProgress -> (Jogo, MapId, Int, MetaProgress)
prepararPartida modoAtual meta =
  let mapaId = mapaParaPartida modoAtual meta
      waves = case modoAtual of
        ModoHistoria -> wavesHistoria meta
        _ -> ondasParaModo modoAtual
      totalOndas = totalOndasPartidaModo modoAtual meta
      metaAtualizado =
        if modoAtual == ModoHistoria
          then meta
          else meta {rotacaoMapasAtual = rotacaoMapasAtual meta + 1}
      jogoNovo =
        Jogo
          { mapaJogo = mapaPorId mapaId,
            baseJogo = baseParaModo modoAtual mapaId,
            portaisJogo = [(portalPorMapa mapaId) {ondasPortal = waves}],
            torresJogo = [],
            inimigosJogo = [],
            lojaJogo = lojaParaModo modoAtual meta
          }
   in (jogoNovo, mapaId, totalOndas, metaAtualizado)
