module TowerSystemSpec (testesTowerSystem) where

import LI12425
import ImmutableTowers (ModoJogoEscolhido (..))
import MetaTypes
import SaveSystem
import Test.HUnit
import TowerRuntime
import TowerSystem

testesTowerSystem :: Test
testesTowerSystem =
  TestLabel "Tower identity and runtime" $
    test
      [ "explicit identity wins over projectile inference" ~: identidadeExplicita,
        "shop entries preserve tower identity" ~: identidadeLoja,
        "maximum tower level blocks further upgrades" ~: nivelMaximo,
        "target priority selects fast enemies for Glaciar" ~: targetingRapido,
        "target priority selects high health enemies for Impacto" ~: targetingVida,
        "high upgrades require one specialization" ~: especializacaoObrigatoria,
        "legacy saves migrate tower identity" ~: migracaoSaveLegado,
        "versioned saves preserve runtime level" ~: roundTripSaveV2
      ]

identidadeExplicita :: Assertion
identidadeExplicita =
  let pos = (3.5, 4.5)
      torre = torreImpactoBase {posicaoTorre = pos}
      registry = registerTower Braseiro pos emptyTowerRegistry
   in towerIdSpec (towerSpecDaTorre registry torre) @?= Braseiro

identidadeLoja :: Assertion
identidadeLoja =
  let meta = progressoInicial {torresDesbloqueadas = [Sentinela, Impacto]}
      entries = shopEntriesParaModo ModoHistoria meta
   in [(shopTowerId entry, shopPrice entry) | entry <- entries] @?= [(Sentinela, 44), (Impacto, 138)]

nivelMaximo :: Assertion
nivelMaximo =
  let spec = towerSpec Sentinela
      runtime = TowerRuntime Sentinela (nivelMaximoTowerSpec spec) Nothing
   in do
        custoUpgradeRuntime runtime torreResinaBase @?= Nothing
        assertBool "Upgrade must be rejected at max level" $
          case upgradeTorreRuntime runtime torreResinaBase of
            Nothing -> True
            Just _ -> False

targetingRapido :: Assertion
targetingRapido =
  let pos = (2.5, 2.5)
      torre = torreGeloBase {posicaoTorre = pos}
      registry = registerTower Glaciar pos emptyTowerRegistry
      lento = inimigoTeste (1.5, 1.5) 1 300
      rapido = inimigoTeste (3.5, 3.5) 4 50
   in case selecionaAlvos registry (10, 10) torre [(1, lento), (2, rapido)] of
        (indice, _) : _ -> indice @?= 2
        [] -> assertFailure "Target selector returned no enemies"

targetingVida :: Assertion
targetingVida =
  let pos = (2.5, 2.5)
      torre = torreImpactoBase {posicaoTorre = pos}
      registry = registerTower Impacto pos emptyTowerRegistry
      fragil = inimigoTeste (1.5, 1.5) 3 80
      tanque = inimigoTeste (3.5, 3.5) 1 500
   in case selecionaAlvos registry (10, 10) torre [(1, fragil), (2, tanque)] of
        (indice, _) : _ -> indice @?= 2
        [] -> assertFailure "Target selector returned no enemies"

especializacaoObrigatoria :: Assertion
especializacaoObrigatoria =
  let runtime = TowerRuntime Sentinela nivelEscolhaEspecializacao Nothing
      baseUpgrade = upgradeTorre torreResinaBase
   in do
        assertBool "Generic upgrade must pause for specialization" $
          case upgradeTorreRuntime runtime torreResinaBase of
            Nothing -> True
            Just _ -> False
        case upgradeComEspecializacao EspecializacaoA runtime torreResinaBase of
          Nothing -> assertFailure "Power specialization should be available"
          Just especializada -> assertBool "Power path must add damage" (danoTorre especializada > danoTorre baseUpgrade)

migracaoSaveLegado :: Assertion
migracaoSaveLegado =
  let pos = (2.5, 2.5)
      torre = torreImpactoBase {posicaoTorre = pos}
      jogoLegado = jogoComTorres [torre]
   in case decodeGameSave (show jogoLegado) of
        Nothing -> assertFailure "Legacy save should be decoded"
        Just (_, registry) ->
          fmap runtimeTowerId (lookupTowerRuntime pos registry) @?= Just Impacto

roundTripSaveV2 :: Assertion
roundTripSaveV2 =
  let pos = (5.5, 6.5)
      torre = torreFogoBase {posicaoTorre = pos}
      runtime = TowerRuntime Braseiro 4 (Just EspecializacaoA)
      registry = insertTowerRuntime pos runtime emptyTowerRegistry
   in case decodeGameSave (encodeGameSave (jogoComTorres [torre]) registry) of
        Nothing -> assertFailure "Versioned save should be decoded"
        Just (_, loadedRegistry) -> lookupTowerRuntime pos loadedRegistry @?= Just runtime

jogoComTorres :: [Torre] -> Jogo
jogoComTorres torres =
  Jogo
    { baseJogo = Base 80 (0.5, 0.5) 150,
      portaisJogo = [],
      torresJogo = torres,
      mapaJogo = [[Terra, Relva], [Relva, Terra]],
      inimigosJogo = [],
      lojaJogo = []
    }

inimigoTeste :: Posicao -> Float -> Float -> Inimigo
inimigoTeste pos velocidade vida =
  Inimigo
    { posicaoInimigo = pos,
      direcaoInimigo = Este,
      vidaInimigo = vida,
      velocidadeBaseInimigo = velocidade,
      velocidadeInimigo = velocidade,
      ataqueInimigo = 10,
      butimInimigo = 10,
      projeteisInimigo = []
    }
