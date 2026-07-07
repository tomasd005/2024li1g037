module TowerSystem
  ( TowerSpec (..),
    torreResinaBase,
    torreGeloBase,
    torreFogoBase,
    torreImpactoBase,
    torreMedoBase,
    torreVenenoBase,
    torreEletricaBase,
    torreSolarBase,
    torreTempestadeBase,
    lojaParaModo,
    upgradeTorre,
    custoUpgradeTorre,
    valorVendaTorre,
    mesmoModeloTorre,
    towerSpec,
    towerSpecsOrdenadas,
    raridadeTorre,
    nomeTorre,
    abrirBau,
    tentaFundirTempestade,
  )
where

import ImmutableTowers (ModoJogoEscolhido (..))
import LI12425
import MetaTypes

data TowerSpec = TowerSpec
  { towerIdSpec :: TowerId,
    nomeTowerSpec :: String,
    raridadeTowerSpec :: Raridade,
    precoTowerSpec :: Creditos,
    torreBaseSpec :: Torre
  }

torreResinaBase, torreGeloBase, torreFogoBase, torreImpactoBase, torreMedoBase, torreVenenoBase, torreEletricaBase, torreSolarBase, torreTempestadeBase :: Torre
torreResinaBase = Torre (0, 0) 16 5.3 1 1.12 0 (Projetil Resina (Finita 3.8))
torreGeloBase = Torre (0, 0) 10 4.5 2 1.68 0 (Projetil Gelo (Finita 1.7))
torreFogoBase = Torre (0, 0) 25 3.9 1 0.98 0 (Projetil Fogo (Finita 2.4))
torreImpactoBase = Torre (0, 0) 40 3.1 1 1.72 0 (Projetil Fogo (Finita 0.8))
torreMedoBase = Torre (0, 0) 8 4.9 1 1.95 0 (Projetil Medo (Finita 2.2))
torreVenenoBase = Torre (0, 0) 9 4.9 2 1.48 0 (Projetil Veneno (Finita 4.5))
torreEletricaBase = Torre (0, 0) 19 4.2 3 2.2 0 (Projetil Eletrico (Finita 1.0))
torreSolarBase = Torre (0, 0) 31 4.6 2 1.24 0 (Projetil Fogo (Finita 2.8))
torreTempestadeBase = Torre (0, 0) 28 5.4 4 1.78 0 (Projetil Eletrico (Finita 1.7))

towerSpec :: TowerId -> TowerSpec
towerSpec towerId = case towerId of
  Sentinela -> TowerSpec Sentinela "SENTINELA" Comum 44 torreResinaBase
  Glaciar -> TowerSpec Glaciar "GLACIAR" Raro 62 torreGeloBase
  Braseiro -> TowerSpec Braseiro "BRASEIRO" Raro 74 torreFogoBase
  Panico -> TowerSpec Panico "PANICO" Epico 92 torreMedoBase
  Venenoide -> TowerSpec Venenoide "VENENOIDE" Epico 106 torreVenenoBase
  Tesla -> TowerSpec Tesla "TESLA" Lendario 122 torreEletricaBase
  Impacto -> TowerSpec Impacto "IMPACTO" Lendario 138 torreImpactoBase
  Solar -> TowerSpec Solar "SOLAR" Lendario 146 torreSolarBase
  Tempestade -> TowerSpec Tempestade "TEMPESTADE" Mitico 188 torreTempestadeBase

towerSpecsOrdenadas :: [TowerSpec]
towerSpecsOrdenadas =
  map towerSpec [Sentinela, Glaciar, Braseiro, Panico, Venenoide, Tesla, Impacto, Solar, Tempestade]

raridadeTorre :: TowerId -> Raridade
raridadeTorre = raridadeTowerSpec . towerSpec

nomeTorre :: TowerId -> String
nomeTorre = nomeTowerSpec . towerSpec

lojaParaModo :: ModoJogoEscolhido -> MetaProgress -> Loja
lojaParaModo modoAtual meta =
  let desconto = case modoAtual of
        ModoSandbox -> 24
        ModoDesafio -> 12
        _ -> 0
      towerIdsDisponiveis = torresDesbloqueadas meta
      precoFinal spec = max 12 (precoTowerSpec spec - desconto)
   in [ (precoFinal spec, torreBaseSpec spec)
        | towerId <- towerIdsDisponiveis,
          let spec = towerSpec towerId
      ]

upgradeTorre :: Torre -> Torre
upgradeTorre torre =
  torre
    { danoTorre = danoTorre torre * 1.28 + 4,
      alcanceTorre = alcanceTorre torre + 0.28,
      rajadaTorre = if danoTorre torre > 38 then min 5 (rajadaTorre torre + 1) else rajadaTorre torre,
      cicloTorre = max 0.4 (cicloTorre torre * 0.9),
      tempoTorre = min (tempoTorre torre) (max 0.4 (cicloTorre torre * 0.9)),
      projetilTorre = melhoraProjetil (projetilTorre torre)
    }

melhoraProjetil :: Projetil -> Projetil
melhoraProjetil projetil = projetil {duracaoProjetil = melhoraDuracao (duracaoProjetil projetil)}
  where
    melhoraDuracao Infinita = Infinita
    melhoraDuracao (Finita t) = Finita (t * 1.18 + 0.3)

custoUpgradeTorre :: Torre -> Creditos
custoUpgradeTorre torre =
  floor (28 + danoTorre torre * 2.15 + alcanceTorre torre * 10 + fromIntegral (rajadaTorre torre * 22) + (2.2 - min 2.0 (cicloTorre torre)) * 42)

valorVendaTorre :: Torre -> Creditos
valorVendaTorre torre =
  max 15 (floor (fromIntegral (custoUpgradeTorre torre) * (0.58 :: Float)))

mesmoModeloTorre :: Torre -> Torre -> Bool
mesmoModeloTorre t1 t2 =
  tipoProjetil (projetilTorre t1) == tipoProjetil (projetilTorre t2)
    && danoTorre t1 == danoTorre t2
    && alcanceTorre t1 == alcanceTorre t2
    && cicloTorre t1 == cicloTorre t2
    && rajadaTorre t1 == rajadaTorre t2

abrirBau :: ChestType -> MetaProgress -> (MetaProgress, String)
abrirBau bau meta
  | gemasJogador meta < custo = (meta, "Gemas insuficientes")
  | null candidatos = (meta {gemasJogador = gemasJogador meta - custo}, "Bau aberto: fragmentos convertidos em ouro")
  | otherwise =
      let indice = (gemsSeed + length (torresDesbloqueadas meta) + estagiosConcluidos meta) `mod` length candidatos
          novaTorre = candidatos !! indice
          novasTorres = torresDesbloqueadas meta ++ [novaTorre]
          nivelNovo = max (nivelJogadorMeta meta) (1 + length novasTorres `div` 2)
          metaNovo =
            meta
              { gemasJogador = gemasJogador meta - custo,
                torresDesbloqueadas = novasTorres,
                nivelJogadorMeta = nivelNovo
              }
       in (metaNovo, "Bau abriu: " ++ nomeTorre novaTorre)
  where
    custo = custoBau bau
    pool = poolDoBau bau
    candidatos = filter (`notElem` torresDesbloqueadas meta) pool
    gemsSeed = gemasJogador meta + rotacaoMapasAtual meta * 7 + nivelJogadorMeta meta * 11

poolDoBau :: ChestType -> [TowerId]
poolDoBau bau = case bau of
  BauMadeira -> [Sentinela, Glaciar, Braseiro, Sentinela, Glaciar]
  BauCristal -> [Glaciar, Braseiro, Panico, Venenoide, Tesla, Impacto]
  BauImperial -> [Panico, Venenoide, Tesla, Impacto, Solar, Tempestade, Solar]

tentaFundirTempestade :: MetaProgress -> Either String MetaProgress
tentaFundirTempestade meta
  | Tempestade `elem` torresDesbloqueadas meta = Left "Tempestade ja desbloqueada"
  | Tesla `notElem` torresDesbloqueadas meta || Solar `notElem` torresDesbloqueadas meta = Left "Falta Tesla e Solar"
  | gemasJogador meta < 180 = Left "Faltam 180 gemas"
  | otherwise =
      Right
        meta
          { gemasJogador = gemasJogador meta - 180,
            torresDesbloqueadas = torresDesbloqueadas meta ++ [Tempestade],
            torresFundidas = Tempestade : torresFundidas meta,
            nivelJogadorMeta = max (nivelJogadorMeta meta) 6
          }
