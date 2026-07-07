module GameFactory
  ( baseParaModo,
    jogoParaModo,
    mapaParaModo,
  )
where

import ImmutableTowers (ModoJogoEscolhido (..))
import LI12425
import MapData
import TowerSystem
import WaveSystem

baseParaModo :: ModoJogoEscolhido -> Base
baseParaModo modoAtual = base01
  { creditosBase = case modoAtual of
      ModoSandbox -> 999
      ModoDesafio -> 115
      ModoBoss -> 180
      ModoInfinito -> 165
      ModoHistoria -> 150,
    vidaBase = case modoAtual of
      ModoDesafio -> 55
      ModoBoss -> 100
      _ -> 80
  }

jogoParaModo :: ModoJogoEscolhido -> Jogo
jogoParaModo modoAtual =
  Jogo
    { mapaJogo = mapaParaModo modoAtual,
      baseJogo = baseParaModo modoAtual,
      portaisJogo = [portalBase {ondasPortal = ondasParaModo modoAtual}],
      torresJogo = [],
      inimigosJogo = [],
      lojaJogo = lojaParaModo modoAtual
    }

mapaParaModo :: ModoJogoEscolhido -> Mapa
mapaParaModo modoAtual = case modoAtual of
  ModoHistoria -> mapa01
  ModoInfinito -> mapa02
  ModoDesafio -> mapa02
  ModoBoss -> mapa01
  ModoSandbox -> mapa01
