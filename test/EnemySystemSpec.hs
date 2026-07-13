module EnemySystemSpec (testesEnemySystem) where

import EnemySystem
import ImmutableTowers (ModoJogoEscolhido (..))
import LI12425
import MetaTypes (TowerId (..))
import Test.HUnit
import TowerRuntime
import TowerSystem
import WaveSystem

testesEnemySystem :: Test
testesEnemySystem =
  TestLabel "Enemy classes and counters" $
    test
      [ "enemy class survives runtime inference" ~: classesRoundTrip,
        "armor reduces direct tower damage" ~: armaduraReduzDano,
        "shield reduces damage above its threshold" ~: escudoReduzDano,
        "regeneration heals without exceeding maximum health" ~: regeneracaoLimitada,
        "accelerator boss changes phase at low health" ~: bossAcelera,
        "guardian boss protects nearby enemies" ~: guardiaoProtege,
        "rupture boss weakens towers inside its danger zone" ~: rupturaCriaZona,
        "boss mode contains three distinct bosses" ~: modoBossCompleto
      ]

classesRoundTrip :: Assertion
classesRoundTrip =
  [enemyClassOf (criaInimigoClasse enemyClass 20) | enemyClass <- [minBound .. maxBound]]
    @?= [minBound .. maxBound]

armaduraReduzDano :: Assertion
armaduraReduzDano =
  let torre = torreImpactoBase {danoTorre = 80}
      registry = registerTower Impacto (posicaoTorre torre) emptyTowerRegistry
      basico = criaInimigoClasse Basico 4
      blindado = criaInimigoClasse Blindado 4
      danoBasico = vidaInimigo basico - vidaInimigo (resolveTowerHit registry torre basico)
      danoBlindado = vidaInimigo blindado - vidaInimigo (resolveTowerHit registry torre blindado)
   in assertBool "Armored enemy should receive less direct damage" (danoBlindado < danoBasico)

escudoReduzDano :: Assertion
escudoReduzDano =
  let torre = torreResinaBase {danoTorre = 60}
      registry = registerTower Sentinela (posicaoTorre torre) emptyTowerRegistry
      protegido = criaInimigoClasse Protegido 3
      semEscudo = protegido {vidaInimigo = vidaMaximaEstimada protegido * 0.5}
      danoComEscudo = vidaInimigo protegido - vidaInimigo (resolveTowerHit registry torre protegido)
      danoSemEscudo = vidaInimigo semEscudo - vidaInimigo (resolveTowerHit registry torre semEscudo)
   in assertBool "Active shield should reduce damage" (danoComEscudo < danoSemEscudo)

regeneracaoLimitada :: Assertion
regeneracaoLimitada =
  let inimigo = criaInimigoClasse Regenerador 2
      maxVida = vidaMaximaEstimada inimigo
      ferido = inimigo {vidaInimigo = maxVida - 2}
      curado = atualizaPassivos 2 ferido
   in do
        assertBool "Regenerator should heal" (vidaInimigo curado > vidaInimigo ferido)
        assertBool "Regenerator must not exceed max health" (vidaInimigo curado <= maxVida)

bossAcelera :: Assertion
bossAcelera =
  let boss = criaInimigoClasse BossAcelerador 5
      ferido = boss {vidaInimigo = vidaMaximaEstimada boss * 0.2}
      atualizado = atualizaPassivos 0 ferido
   in assertBool "Low-health accelerator boss should speed up" (velocidadeInimigo atualizado > velocidadeInimigo ferido)

guardiaoProtege :: Assertion
guardiaoProtege =
  let torre = torreImpactoBase {posicaoTorre = (0, 0), danoTorre = 80}
      registry = registerTower Impacto (posicaoTorre torre) emptyTowerRegistry
      alvo = (criaInimigoClasse Basico 4) {posicaoInimigo = (4, 4)}
      guardiao = (criaInimigoClasse BossGuardiao 4) {posicaoInimigo = (6, 4)}
      danoNormal = vidaInimigo alvo - vidaInimigo (resolveTowerHitComContexto registry (contextoCombateInimigos []) torre alvo)
      danoProtegido = vidaInimigo alvo - vidaInimigo (resolveTowerHitComContexto registry (contextoCombateInimigos [guardiao]) torre alvo)
   in assertBool "Guardian aura should reduce incoming damage" (danoProtegido < danoNormal)

rupturaCriaZona :: Assertion
rupturaCriaZona =
  let torre = torreImpactoBase {posicaoTorre = (2, 2), danoTorre = 80}
      registry = registerTower Impacto (posicaoTorre torre) emptyTowerRegistry
      alvo = (criaInimigoClasse Basico 4) {posicaoInimigo = (4, 4)}
      ruptura = (criaInimigoClasse BossRuptura 4) {posicaoInimigo = (3, 2)}
      danoNormal = vidaInimigo alvo - vidaInimigo (resolveTowerHitComContexto registry (contextoCombateInimigos []) torre alvo)
      danoNaZona = vidaInimigo alvo - vidaInimigo (resolveTowerHitComContexto registry (contextoCombateInimigos [ruptura]) torre alvo)
   in assertBool "Rupture danger zone should reduce tower damage" (danoNaZona < danoNormal)

modoBossCompleto :: Assertion
modoBossCompleto =
  let classes = map enemyClassOf (concatMap inimigosOnda (ondasParaModo ModoBoss))
   in assertBool "Boss mode should include all three boss identities" $
        all (`elem` classes) [BossAcelerador, BossGuardiao, BossRuptura]
