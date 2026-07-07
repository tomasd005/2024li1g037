module MetaTypes
  ( MapId (..),
    Raridade (..),
    TowerId (..),
    ChestType (..),
    MetaProgress (..),
    progressoInicial,
    nomeMapa,
    nomeRaridade,
    nomeTowerId,
    custoBau,
  )
where

data MapId
  = PlanicieSerena
  | GargantaPedra
  | LagoFraturado
  | CruzamentoSolar
  | BastiaoEspiral
  deriving (Show, Read, Eq, Enum, Bounded)

data Raridade = Comum | Raro | Epico | Lendario | Mitico
  deriving (Show, Read, Eq, Enum, Bounded, Ord)

data TowerId
  = Sentinela
  | Glaciar
  | Braseiro
  | Panico
  | Venenoide
  | Tesla
  | Impacto
  | Solar
  | Tempestade
  deriving (Show, Read, Eq, Enum, Bounded, Ord)

data ChestType = BauMadeira | BauCristal | BauImperial
  deriving (Show, Read, Eq, Enum, Bounded)

data MetaProgress = MetaProgress
  { gemasJogador :: Int,
    nivelJogadorMeta :: Int,
    torresDesbloqueadas :: [TowerId],
    torresFundidas :: [TowerId],
    capituloHistoriaAtual :: Int,
    estagioHistoriaAtual :: Int,
    estagiosConcluidos :: Int,
    rotacaoMapasAtual :: Int
  }
  deriving (Show, Read, Eq)

progressoInicial :: MetaProgress
progressoInicial =
  MetaProgress
    { gemasJogador = 40,
      nivelJogadorMeta = 1,
      torresDesbloqueadas = [Sentinela],
      torresFundidas = [],
      capituloHistoriaAtual = 1,
      estagioHistoriaAtual = 1,
      estagiosConcluidos = 0,
      rotacaoMapasAtual = 0
    }

nomeMapa :: MapId -> String
nomeMapa mapaId = case mapaId of
  PlanicieSerena -> "PLANICIE SERENA"
  GargantaPedra -> "GARGANTA DE PEDRA"
  LagoFraturado -> "LAGO FRATURADO"
  CruzamentoSolar -> "CRUZAMENTO SOLAR"
  BastiaoEspiral -> "BASTIAO ESPIRAL"

nomeRaridade :: Raridade -> String
nomeRaridade raridade = case raridade of
  Comum -> "COMUM"
  Raro -> "RARO"
  Epico -> "EPICO"
  Lendario -> "LENDARIO"
  Mitico -> "MITICO"

nomeTowerId :: TowerId -> String
nomeTowerId towerId = case towerId of
  Sentinela -> "SENTINELA"
  Glaciar -> "GLACIAR"
  Braseiro -> "BRASEIRO"
  Panico -> "PANICO"
  Venenoide -> "VENENOIDE"
  Tesla -> "TESLA"
  Impacto -> "IMPACTO"
  Solar -> "SOLAR"
  Tempestade -> "TEMPESTADE"

custoBau :: ChestType -> Int
custoBau bau = case bau of
  BauMadeira -> 35
  BauCristal -> 90
  BauImperial -> 170
