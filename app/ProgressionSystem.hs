module ProgressionSystem
  ( dadosCampanha,
    nivelMinimoModo,
    modoDesbloqueado,
    recompensaVitoriaModo,
    avancaHistoria,
    mapaParaPartida,
    wavesHistoria,
    totalOndasPartidaModo,
    capituloEstagioTexto,
  )
where

import ImmutableTowers
import LI12425
import MetaTypes
import WaveSystem (criaOnda, ondasParaModo)

dadosCampanha :: MetaProgress -> (Int, Int)
dadosCampanha meta = (capituloHistoriaAtual meta, estagioHistoriaAtual meta)

nivelMinimoModo :: ModoJogoEscolhido -> Int
nivelMinimoModo modoAtual = case modoAtual of
  ModoHistoria -> 1
  ModoInfinito -> 2
  ModoDesafio -> 4
  ModoBoss -> 6
  ModoSandbox -> 3

modoDesbloqueado :: MetaProgress -> ModoJogoEscolhido -> Bool
modoDesbloqueado meta modoAtual =
  nivelJogadorMeta meta >= nivelMinimoModo modoAtual

recompensaVitoriaModo :: ModoJogoEscolhido -> MetaProgress -> Int
recompensaVitoriaModo modoAtual meta =
  case modoAtual of
    ModoHistoria ->
      let (capitulo, estagio) = dadosCampanha meta
       in 24 + capitulo * 10 + estagio * 6
    ModoInfinito -> 18 + min 60 (estagiosConcluidos meta * 2)
    ModoDesafio -> 42
    ModoBoss -> 60
    ModoSandbox -> 8

avancaHistoria :: MetaProgress -> MetaProgress
avancaHistoria meta =
  let concluidos = estagiosConcluidos meta + 1
      nivelNovo = max (nivelJogadorMeta meta) (1 + concluidos `div` 2)
      capituloAtual = capituloHistoriaAtual meta
      estagioAtual = estagioHistoriaAtual meta
      (novoCapitulo, novoEstagio)
        | estagioAtual < 5 = (capituloAtual, estagioAtual + 1)
        | otherwise = (capituloAtual + 1, 1)
   in meta
        { capituloHistoriaAtual = novoCapitulo,
          estagioHistoriaAtual = novoEstagio,
          estagiosConcluidos = concluidos,
          nivelJogadorMeta = nivelNovo
        }

mapaParaPartida :: ModoJogoEscolhido -> MetaProgress -> MapId
mapaParaPartida modoAtual meta = case modoAtual of
  ModoHistoria ->
    let (_, estagio) = dadosCampanha meta
     in rodaMapa (estagio - 1)
  _ -> rodaMapa (rotacaoMapasAtual meta)

rodaMapa :: Int -> MapId
rodaMapa indice =
  case indice `mod` 5 of
    0 -> PlanicieSerena
    1 -> GargantaPedra
    2 -> LagoFraturado
    3 -> CruzamentoSolar
    _ -> BastiaoEspiral

wavesHistoria :: MetaProgress -> [Onda]
wavesHistoria meta =
  let (capitulo, estagio) = dadosCampanha meta
      baseNivel = (capitulo - 1) * 4 + estagio
   in [criaOnda (baseNivel + deslocamento) (4 + baseNivel + deslocamento) (fromIntegral deslocamento * 2.2) | deslocamento <- [0 .. 9]]

totalOndasPartidaModo :: ModoJogoEscolhido -> MetaProgress -> Int
totalOndasPartidaModo modoAtual meta = case modoAtual of
  ModoHistoria -> 10
  ModoInfinito -> 1
  _ -> length (ondasParaModo modoAtual)

capituloEstagioTexto :: MetaProgress -> String
capituloEstagioTexto meta =
  "CAP " ++ show (capituloHistoriaAtual meta) ++ "  EST " ++ show (estagioHistoriaAtual meta)
