module TowerSystem
  ( TowerRole (..),
    TowerTag (..),
    TargetPriority (..),
    TowerShape (..),
    TowerSpec (..),
    ShopEntry (..),
    torreResinaBase,
    torreGeloBase,
    torreFogoBase,
    torreImpactoBase,
    torreMedoBase,
    torreVenenoBase,
    torreEletricaBase,
    torreSolarBase,
    torreTempestadeBase,
    shopEntriesParaModo,
    lojaParaModo,
    upgradeTorre,
    upgradeTorreRuntime,
    upgradeComEspecializacao,
    custoUpgradeTorre,
    custoUpgradeRuntime,
    custoEspecializacao,
    precisaEspecializacao,
    nivelEscolhaEspecializacao,
    nomeEspecializacao,
    descricaoEspecializacao,
    valorVendaTorre,
    valorVendaRuntime,
    mesmoModeloTorre,
    towerSpec,
    towerSpecAproximada,
    towerSpecDaTorre,
    towerRuntimeDaTorre,
    registryFromLegacy,
    reconcileTowerRegistry,
    selecionaAlvos,
    towerSpecsOrdenadas,
    raridadeTorre,
    nomeTorre,
    raridadeTorreAproximada,
    abrirBau,
    tentaFundirTempestade,
  )
where

import Data.Function (on)
import Data.List (minimumBy, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import ImmutableTowers (ModoJogoEscolhido (..))
import LI12425
import MetaTypes
import TowerRuntime

data TowerRole
  = DanoConsistente
  | Controlo
  | DanoContinuo
  | ControloRota
  | Execucao
  | AntiEnxame
  | RajadaPesada
  | SuporteOfensivo
  | Endgame
  deriving (Show, Read, Eq)

data TowerTag
  = SingleTarget
  | Area
  | Slow
  | Burn
  | Poison
  | Fear
  | Chain
  | Burst
  | Support
  deriving (Show, Read, Eq)

data TargetPriority
  = PrimeiroNaRota
  | MaisRapido
  | MaisVida
  | MaiorGrupo
  deriving (Show, Read, Eq)

data TowerShape
  = FormaSentinela
  | FormaCristal
  | FormaBraseiro
  | FormaOrbe
  | FormaFrasco
  | FormaBobina
  | FormaCanhao
  | FormaSolar
  | FormaTempestade
  deriving (Show, Read, Eq)

data TowerSpec = TowerSpec
  { towerIdSpec :: !TowerId,
    nomeTowerSpec :: String,
    raridadeTowerSpec :: !Raridade,
    papelTowerSpec :: !TowerRole,
    descricaoTowerSpec :: String,
    tagsTowerSpec :: [TowerTag],
    prioridadeTowerSpec :: !TargetPriority,
    corTowerSpec :: !(Int, Int, Int),
    formaTowerSpec :: !TowerShape,
    areaTowerSpec :: !Float,
    nivelMaximoTowerSpec :: !Int,
    precoTowerSpec :: !Creditos,
    torreBaseSpec :: !Torre
  }
  deriving (Show, Read)

data ShopEntry = ShopEntry
  { shopTowerId :: !TowerId,
    shopPrice :: !Creditos,
    shopTower :: !Torre
  }
  deriving (Show, Read)

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
  Sentinela -> TowerSpec Sentinela "SENTINELA" Comum DanoConsistente "Barata e fiavel contra alvos isolados." [SingleTarget] PrimeiroNaRota (145, 107, 61) FormaSentinela 0 4 44 torreResinaBase
  Glaciar -> TowerSpec Glaciar "GLACIAR" Raro Controlo "Atrasa inimigos rapidos e cria tempo para reagir." [Slow, Area] MaisRapido (111, 150, 168) FormaCristal 0.6 5 62 torreGeloBase
  Braseiro -> TowerSpec Braseiro "BRASEIRO" Raro DanoContinuo "Queima grupos e recompensa rotas congestionadas." [Burn, Area] MaiorGrupo (175, 88, 58) FormaBraseiro 0.8 5 74 torreFogoBase
  Panico -> TowerSpec Panico "PANICO" Epico ControloRota "Faz recuar inimigos, mas pode atrasar o foco de outras torres." [Fear, SingleTarget] PrimeiroNaRota (170, 132, 210) FormaOrbe 0 5 92 torreMedoBase
  Venenoide -> TowerSpec Venenoide "VENENOIDE" Epico Execucao "Desgasta alvos resistentes durante varios segundos." [Poison, SingleTarget] MaisVida (101, 168, 92) FormaFrasco 0 5 106 torreVenenoBase
  Tesla -> TowerSpec Tesla "TESLA" Lendario AntiEnxame "Atinge varios inimigos e controla enxames." [Chain, Area] MaiorGrupo (226, 194, 95) FormaBobina 1.1 6 122 torreEletricaBase
  Impacto -> TowerSpec Impacto "IMPACTO" Lendario RajadaPesada "Canhao lento para eliminar inimigos de muita vida." [Burst, SingleTarget] MaisVida (190, 126, 74) FormaCanhao 0.5 6 138 torreImpactoBase
  Solar -> TowerSpec Solar "SOLAR" Lendario SuporteOfensivo "Dano versatil com janelas fortes de ataque." [Support, Burn] PrimeiroNaRota (235, 184, 72) FormaSolar 0.7 6 146 torreSolarBase
  Tempestade -> TowerSpec Tempestade "TEMPESTADE" Mitico Endgame "Fusao de alto investimento para dominar grandes vagas." [Chain, Area, Burst] MaiorGrupo (132, 184, 220) FormaTempestade 1.4 7 188 torreTempestadeBase

towerSpecsOrdenadas :: [TowerSpec]
towerSpecsOrdenadas = map towerSpec [Sentinela, Glaciar, Braseiro, Panico, Venenoide, Tesla, Impacto, Solar, Tempestade]

raridadeTorre :: TowerId -> Raridade
raridadeTorre = raridadeTowerSpec . towerSpec

nomeTorre :: TowerId -> String
nomeTorre = nomeTowerSpec . towerSpec

towerSpecAproximada :: Torre -> TowerSpec
towerSpecAproximada torre =
  minimumBy (compare `on` distanciaSpec torre) candidatos
  where
    mesmoProjetil spec = tipoProjetil (projetilTorre (torreBaseSpec spec)) == tipoProjetil (projetilTorre torre)
    candidatosProjetil = filter mesmoProjetil towerSpecsOrdenadas
    candidatos = if null candidatosProjetil then towerSpecsOrdenadas else candidatosProjetil

distanciaSpec :: Torre -> TowerSpec -> Float
distanciaSpec torre spec =
  let base = torreBaseSpec spec
   in abs (danoTorre torre - danoTorre base)
        + abs (alcanceTorre torre - alcanceTorre base) * 4
        + abs (cicloTorre torre - cicloTorre base) * 6
        + fromIntegral (abs (rajadaTorre torre - rajadaTorre base)) * 8

raridadeTorreAproximada :: Torre -> Raridade
raridadeTorreAproximada = raridadeTowerSpec . towerSpecAproximada

shopEntriesParaModo :: ModoJogoEscolhido -> MetaProgress -> [ShopEntry]
shopEntriesParaModo modoAtual meta =
  [ ShopEntry towerId (precoFinal spec) (torreBaseSpec spec)
    | towerId <- torresDesbloqueadas meta,
      let spec = towerSpec towerId
  ]
  where
    desconto = case modoAtual of
      ModoSandbox -> 24
      ModoDesafio -> 12
      _ -> 0
    precoFinal spec = max 12 (precoTowerSpec spec - desconto)

lojaParaModo :: ModoJogoEscolhido -> MetaProgress -> Loja
lojaParaModo modoAtual meta =
  [(shopPrice entry, shopTower entry) | entry <- shopEntriesParaModo modoAtual meta]

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

upgradeTorreRuntime :: TowerRuntime -> Torre -> Maybe Torre
upgradeTorreRuntime runtime torre
  | runtimeLevel runtime >= nivelMaximoTowerSpec spec = Nothing
  | precisaEspecializacao runtime = Nothing
  | otherwise = Just (aplicaEspecializacao (runtimeSpecialization runtime) (upgradeTorre torre))
  where
    spec = towerSpec (runtimeTowerId runtime)

nivelEscolhaEspecializacao :: Int
nivelEscolhaEspecializacao = 3

precisaEspecializacao :: TowerRuntime -> Bool
precisaEspecializacao runtime =
  runtimeLevel runtime >= nivelEscolhaEspecializacao
    && runtimeSpecialization runtime == Nothing

upgradeComEspecializacao :: TowerSpecialization -> TowerRuntime -> Torre -> Maybe Torre
upgradeComEspecializacao specialization runtime torre
  | not (precisaEspecializacao runtime) = Nothing
  | runtimeLevel runtime >= nivelMaximoTowerSpec (towerSpec (runtimeTowerId runtime)) = Nothing
  | otherwise = Just (aplicaEspecializacao (Just specialization) (upgradeTorre torre))

nomeEspecializacao :: TowerSpecialization -> String
nomeEspecializacao specialization = case specialization of
  EspecializacaoA -> "POTENCIA"
  EspecializacaoB -> "CADENCIA"

descricaoEspecializacao :: TowerSpecialization -> String
descricaoEspecializacao specialization = case specialization of
  EspecializacaoA -> "+12% dano e +0.18 alcance por upgrade"
  EspecializacaoB -> "+1 rajada e -14% ciclo por upgrade"

aplicaEspecializacao :: Maybe TowerSpecialization -> Torre -> Torre
aplicaEspecializacao Nothing torre = torre
aplicaEspecializacao (Just EspecializacaoA) torre =
  torre {danoTorre = danoTorre torre * 1.12, alcanceTorre = alcanceTorre torre + 0.18}
aplicaEspecializacao (Just EspecializacaoB) torre =
  torre {cicloTorre = max 0.32 (cicloTorre torre * 0.86), rajadaTorre = min 6 (rajadaTorre torre + 1)}

melhoraProjetil :: Projetil -> Projetil
melhoraProjetil projetil = projetil {duracaoProjetil = melhoraDuracao (duracaoProjetil projetil)}
  where
    melhoraDuracao Infinita = Infinita
    melhoraDuracao (Finita t) = Finita (t * 1.18 + 0.3)

custoUpgradeTorre :: Torre -> Creditos
custoUpgradeTorre torre =
  floor (28 + danoTorre torre * 2.15 + alcanceTorre torre * 10 + fromIntegral (rajadaTorre torre * 22) + (2.2 - min 2.0 (cicloTorre torre)) * 42)

custoUpgradeRuntime :: TowerRuntime -> Torre -> Maybe Creditos
custoUpgradeRuntime runtime torre
  | runtimeLevel runtime >= nivelMaximoTowerSpec (towerSpec (runtimeTowerId runtime)) = Nothing
  | precisaEspecializacao runtime = Nothing
  | otherwise = Just (custoUpgradeTorre torre)

custoEspecializacao :: TowerSpecialization -> TowerRuntime -> Torre -> Maybe Creditos
custoEspecializacao specialization runtime torre
  | not (precisaEspecializacao runtime) = Nothing
  | otherwise =
      let multiplicador = case specialization of
            EspecializacaoA -> 1.18 :: Float
            EspecializacaoB -> 1.12
       in Just (ceiling (fromIntegral (custoUpgradeTorre torre) * multiplicador))

valorVendaTorre :: Torre -> Creditos
valorVendaTorre torre = max 15 (floor (fromIntegral (custoUpgradeTorre torre) * (0.58 :: Float)))

valorVendaRuntime :: TowerRuntime -> Torre -> Creditos
valorVendaRuntime runtime torre =
  let compra = precoTowerSpec (towerSpec (runtimeTowerId runtime))
      investimentoUpgrade = sum (take (max 0 (runtimeLevel runtime - 1)) (iterate (\c -> floor (fromIntegral c * (1.24 :: Float))) (custoUpgradeTorre (torreBaseSpec (towerSpec (runtimeTowerId runtime))))))
   in max 15 (floor (fromIntegral (compra + investimentoUpgrade) * (0.62 :: Float)) + valorVendaTorre torre `div` 8)

mesmoModeloTorre :: Torre -> Torre -> Bool
mesmoModeloTorre t1 t2 =
  tipoProjetil (projetilTorre t1) == tipoProjetil (projetilTorre t2)
    && danoTorre t1 == danoTorre t2
    && alcanceTorre t1 == alcanceTorre t2
    && cicloTorre t1 == cicloTorre t2
    && rajadaTorre t1 == rajadaTorre t2

towerRuntimeDaTorre :: TowerRegistry -> Torre -> TowerRuntime
towerRuntimeDaTorre registry torre =
  fromMaybe legado (lookupTowerRuntime (posicaoTorre torre) registry)
  where
    spec = towerSpecAproximada torre
    legado = TowerRuntime (towerIdSpec spec) (nivelAproximado spec torre) Nothing

towerSpecDaTorre :: TowerRegistry -> Torre -> TowerSpec
towerSpecDaTorre registry = towerSpec . runtimeTowerId . towerRuntimeDaTorre registry

registryFromLegacy :: [Torre] -> TowerRegistry
registryFromLegacy = foldl' regista emptyTowerRegistry
  where
    regista registry torre =
      let spec = towerSpecAproximada torre
          runtime = TowerRuntime (towerIdSpec spec) (nivelAproximado spec torre) Nothing
       in insertTowerRuntime (posicaoTorre torre) runtime registry

reconcileTowerRegistry :: TowerRegistry -> [Torre] -> TowerRegistry
reconcileTowerRegistry registry = foldl' regista emptyTowerRegistry
  where
    regista novoRegistry torre =
      let inferido = towerRuntimeDaTorre emptyTowerRegistry torre
          preservado = fromMaybe inferido (lookupTowerRuntime (posicaoTorre torre) registry)
          runtime = preservado {runtimeLevel = max (runtimeLevel preservado) (runtimeLevel inferido)}
       in insertTowerRuntime (posicaoTorre torre) runtime novoRegistry

selecionaAlvos :: TowerRegistry -> Posicao -> Torre -> [(Int, Inimigo)] -> [(Int, Inimigo)]
selecionaAlvos registry posBase torre candidatos =
  case prioridadeTowerSpec (towerSpecDaTorre registry torre) of
    PrimeiroNaRota -> sortBy (compare `on` distanciaBase) candidatos
    MaisRapido -> sortBy (compare `on` chaveRapido) candidatos
    MaisVida -> sortBy (compare `on` chaveVida) candidatos
    MaiorGrupo ->
      let centro = centroCandidatos candidatos
       in sortBy (compare `on` chaveGrupo centro) candidatos
  where
    inimigoDe = snd
    distanciaBase = distanciaPosicoes posBase . posicaoInimigo . inimigoDe
    chaveRapido alvo = (Down (velocidadeInimigo (inimigoDe alvo)), distanciaBase alvo)
    chaveVida alvo = (Down (vidaInimigo (inimigoDe alvo)), distanciaBase alvo)
    chaveGrupo centro alvo = (distanciaPosicoes centro (posicaoInimigo (inimigoDe alvo)), distanciaBase alvo)

centroCandidatos :: [(Int, Inimigo)] -> Posicao
centroCandidatos [] = (0, 0)
centroCandidatos candidatos =
  let (somaX, somaY, total) = foldl' acumula (0, 0, 0 :: Int) candidatos
      divisor = fromIntegral total
   in (somaX / divisor, somaY / divisor)
  where
    acumula (somaX, somaY, total) (_, inimigo) =
      let (x, y) = posicaoInimigo inimigo
       in (somaX + x, somaY + y, total + 1)

distanciaPosicoes :: Posicao -> Posicao -> Float
distanciaPosicoes (x1, y1) (x2, y2) =
  let dx = x1 - x2
      dy = y1 - y2
   in dx * dx + dy * dy

nivelAproximado :: TowerSpec -> Torre -> Int
nivelAproximado spec torre =
  fst $
    minimumBy (compare `on` snd)
      [ (nivel, distanciaTorre candidato torre)
        | (nivel, candidato) <- zip [1 .. nivelMaximoTowerSpec spec] (iterate upgradeTorre (torreBaseSpec spec))
      ]

distanciaTorre :: Torre -> Torre -> Float
distanciaTorre a b =
  abs (danoTorre a - danoTorre b)
    + abs (alcanceTorre a - alcanceTorre b) * 4
    + abs (cicloTorre a - cicloTorre b) * 6
    + fromIntegral (abs (rajadaTorre a - rajadaTorre b)) * 8

abrirBau :: ChestType -> MetaProgress -> (MetaProgress, String)
abrirBau bau meta
  | gemasJogador meta < custo = (meta, "Gemas insuficientes")
  | null candidatos = (meta {gemasJogador = gemasJogador meta - custo}, "Bau aberto: fragmentos convertidos em ouro")
  | otherwise =
      let indice = (gemsSeed + length (torresDesbloqueadas meta) + estagiosConcluidos meta) `mod` length candidatos
       in case drop indice candidatos of
            novaTorre : _ ->
              let novasTorres = torresDesbloqueadas meta ++ [novaTorre]
                  nivelNovo = max (nivelJogadorMeta meta) (1 + length novasTorres `div` 2)
                  metaNovo = meta {gemasJogador = gemasJogador meta - custo, torresDesbloqueadas = novasTorres, nivelJogadorMeta = nivelNovo}
               in (metaNovo, "Bau abriu: " ++ nomeTorre novaTorre)
            [] -> (meta, "Bau indisponivel")
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
