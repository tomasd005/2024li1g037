module EnemySystem
  ( EnemyClass (..),
    EnemyTag (..),
    EnemyShape (..),
    EnemySpec (..),
    EnemyCombatContext,
    enemySpec,
    enemyClassOf,
    nomeEnemyClass,
    criaInimigoClasse,
    vidaMaximaEstimada,
    resolveTowerHit,
    resolveTowerHitComContexto,
    contextoCombateInimigos,
    atualizaPassivos,
    raioAuraGuardiao,
    raioZonaRuptura,
  )
where

import Data.Function (on)
import Data.List (minimumBy)
import LI12425
import Tarefa2 (atingeInimigo)
import TowerRuntime (TowerRegistry)
import TowerSystem

data EnemyClass
  = Basico
  | Rapido
  | Tanque
  | Blindado
  | Regenerador
  | Dispersor
  | Protegido
  | Elite
  | BossAcelerador
  | BossGuardiao
  | BossRuptura
  deriving (Show, Read, Eq, Enum, Bounded)

data EnemyTag
  = Enxame
  | Pesado
  | Armadura
  | Cura
  | AntiArea
  | Escudo
  | Chefe
  deriving (Show, Read, Eq)

data EnemyShape
  = FormaRedonda
  | FormaVeloz
  | FormaPesada
  | FormaBlindada
  | FormaRegeneradora
  | FormaDispersora
  | FormaEscudo
  | FormaElite
  | FormaBossA
  | FormaBossG
  | FormaBossR
  deriving (Show, Read, Eq)

data EnemySpec = EnemySpec
  { enemyClassSpec :: !EnemyClass,
    nomeEnemySpec :: String,
    tagsEnemySpec :: [EnemyTag],
    corEnemySpec :: !(Int, Int, Int),
    formaEnemySpec :: !EnemyShape,
    vidaBaseEnemySpec :: !Float,
    velocidadeBaseEnemySpec :: !Float,
    ataqueBaseEnemySpec :: !Float,
    butimBaseEnemySpec :: !Int,
    armaduraEnemySpec :: !Float,
    resistenciaDiretaEnemySpec :: !Float,
    resistenciaAreaEnemySpec :: !Float,
    regeneracaoEnemySpec :: !Float,
    escudoEnemySpec :: !Float,
    ameacaEnemySpec :: !Int
  }
  deriving (Show, Read)

data EnemyCombatContext = EnemyCombatContext
  { posicoesGuardioes :: [Posicao],
    posicoesRupturas :: [Posicao]
  }

enemySpec :: EnemyClass -> EnemySpec
enemySpec enemyClass = case enemyClass of
  Basico -> spec "BASICO" [Enxame] (128, 93, 72) FormaRedonda 68 1.02 8 14 0 0 0 0 0 1
  Rapido -> spec "RAPIDO" [Enxame] (218, 166, 74) FormaVeloz 46 2.50 6 13 0 0 0 0 0 1
  Tanque -> spec "TANQUE" [Pesado] (116, 87, 74) FormaPesada 190 0.62 16 24 1 0.08 0 0 0 3
  Blindado -> spec "BLINDADO" [Pesado, Armadura] (128, 138, 142) FormaBlindada 150 0.82 13 25 7 0.16 0 0 0 4
  Regenerador -> spec "REGENERADOR" [Cura] (89, 164, 105) FormaRegeneradora 126 1.24 11 23 1 0.05 0 4.5 0 4
  Dispersor -> spec "DISPERSOR" [AntiArea] (160, 111, 181) FormaDispersora 112 1.46 10 22 0 0.05 0.58 0 0 4
  Protegido -> spec "PROTEGIDO" [Escudo] (87, 151, 187) FormaEscudo 142 1.68 12 27 2 0.08 0.12 0 0.48 5
  Elite -> spec "ELITE" [Pesado, Cura] (207, 111, 67) FormaElite 260 1.90 22 38 4 0.14 0.18 2.5 0 7
  BossAcelerador -> spec "ARIETE VELOZ" [Chefe] (218, 112, 72) FormaBossA 1100 0.26 38 170 5 0.12 0.1 0 0 22
  BossGuardiao -> spec "BASTIAO VIVO" [Chefe, Armadura, Escudo] (105, 142, 166) FormaBossG 1500 0.38 46 210 12 0.22 0.18 0 0.55 28
  BossRuptura -> spec "NEXO DA RUPTURA" [Chefe, Cura, AntiArea] (165, 101, 184) FormaBossR 1280 0.50 52 230 6 0.15 0.48 8 0 30
  where
    spec nome tags cor forma vida velocidade ataque butim armadura resistenciaDireta resistenciaArea regeneracao escudo ameaca =
      EnemySpec enemyClass nome tags cor forma vida velocidade ataque butim armadura resistenciaDireta resistenciaArea regeneracao escudo ameaca

nomeEnemyClass :: EnemyClass -> String
nomeEnemyClass = nomeEnemySpec . enemySpec

criaInimigoClasse :: EnemyClass -> Int -> Inimigo
criaInimigoClasse enemyClass nivel =
  let spec = enemySpec enemyClass
      nivelSeguro = max 1 nivel
      escala = fromIntegral (nivelSeguro - 1)
      vida = vidaBaseEnemySpec spec * (1 + escala * 0.22)
      velocidade = velocidadeBaseEnemySpec spec + min 0.05 (escala * 0.003)
      ataque = ataqueBaseEnemySpec spec * (1 + escala * 0.12)
      butim = floor (fromIntegral (butimBaseEnemySpec spec) * (1 + escala * 0.08))
   in Inimigo
        { posicaoInimigo = (0, 2),
          direcaoInimigo = Este,
          vidaInimigo = vida,
          velocidadeBaseInimigo = velocidade,
          velocidadeInimigo = velocidade,
          ataqueInimigo = ataque,
          butimInimigo = butim,
          projeteisInimigo = []
        }

enemyClassOf :: Inimigo -> EnemyClass
enemyClassOf inimigo =
  enemyClassSpec $
    minimumBy
      (compare `on` (\spec -> abs (velocidadeBaseInimigo inimigo - velocidadeBaseEnemySpec spec)))
      allEnemySpecs

allEnemySpecs :: [EnemySpec]
allEnemySpecs = map enemySpec [minBound .. maxBound]

vidaMaximaEstimada :: Inimigo -> Float
vidaMaximaEstimada inimigo =
  let spec = enemySpec (enemyClassOf inimigo)
      nivel = nivelEstimado spec inimigo
   in vidaBaseEnemySpec spec * (1 + fromIntegral (nivel - 1) * 0.22)

nivelEstimado :: EnemySpec -> Inimigo -> Int
nivelEstimado spec inimigo
  | ataqueBaseEnemySpec spec <= 0 = 1
  | otherwise =
      max 1 $
        1 + round (((ataqueInimigo inimigo / ataqueBaseEnemySpec spec) - 1) / 0.12)

resolveTowerHit :: TowerRegistry -> Torre -> Inimigo -> Inimigo
resolveTowerHit registry torre inimigo =
  let towerData = towerSpecDaTorre registry torre
      enemyData = enemySpec (enemyClassOf inimigo)
      danoInicial = max 0 (danoTorre torre - armaduraEnemySpec enemyData)
      resistenciaTipo = if areaTowerSpec towerData > 0 then resistenciaAreaEnemySpec enemyData else resistenciaDiretaEnemySpec enemyData
      vidaMaxima = vidaMaximaEstimada inimigo
      escudoAtivo = escudoEnemySpec enemyData > 0 && vidaInimigo inimigo > vidaMaxima * 0.66
      multiplicadorEscudo = if escudoAtivo then 1 - escudoEnemySpec enemyData else 1
      danoFinal = max 0.5 (danoInicial * (1 - resistenciaTipo) * multiplicadorEscudo)
   in atingeInimigo torre {danoTorre = danoFinal} inimigo

raioAuraGuardiao, raioZonaRuptura :: Float
raioAuraGuardiao = 3.2
raioZonaRuptura = 4.5

contextoCombateInimigos :: [Inimigo] -> EnemyCombatContext
contextoCombateInimigos = foldr adiciona (EnemyCombatContext [] [])
  where
    adiciona inimigo contexto
      | vidaInimigo inimigo <= 0 = contexto
      | enemyClassOf inimigo == BossGuardiao = contexto {posicoesGuardioes = posicaoInimigo inimigo : posicoesGuardioes contexto}
      | enemyClassOf inimigo == BossRuptura = contexto {posicoesRupturas = posicaoInimigo inimigo : posicoesRupturas contexto}
      | otherwise = contexto

resolveTowerHitComContexto :: TowerRegistry -> EnemyCombatContext -> Torre -> Inimigo -> Inimigo
resolveTowerHitComContexto registry contexto torre inimigo =
  let atingido = resolveTowerHit registry torre inimigo
      danoCalculado = max 0 (vidaInimigo inimigo - vidaInimigo atingido)
      protegidoPeloGuardiao =
        enemyClassOf inimigo /= BossGuardiao
          && existePosicaoPerto (posicoesGuardioes contexto) raioAuraGuardiao (posicaoInimigo inimigo)
      torreNaZonaRuptura = existePosicaoPerto (posicoesRupturas contexto) raioZonaRuptura (posicaoTorre torre)
      multiplicadorGuardiao = if protegidoPeloGuardiao then 0.58 else 1
      multiplicadorRuptura = if torreNaZonaRuptura then 0.7 else 1
      danoFinal = danoCalculado * multiplicadorGuardiao * multiplicadorRuptura
   in atingido {vidaInimigo = max 0 (vidaInimigo inimigo - danoFinal)}
  where
    existePosicaoPerto posicoes raio posicao =
      any (\posicaoBoss -> distanciaQuadrada posicao posicaoBoss <= raio * raio) posicoes

distanciaQuadrada :: Posicao -> Posicao -> Float
distanciaQuadrada (x1, y1) (x2, y2) =
  let dx = x1 - x2
      dy = y1 - y2
   in dx * dx + dy * dy

atualizaPassivos :: Tempo -> Inimigo -> Inimigo
atualizaPassivos tempo inimigo =
  let spec = enemySpec (enemyClassOf inimigo)
      vidaMaxima = vidaMaximaEstimada inimigo
      vidaNova = min vidaMaxima (vidaInimigo inimigo + regeneracaoEnemySpec spec * max 0 tempo)
      percentagemVida = if vidaMaxima <= 0 then 0 else vidaNova / vidaMaxima
      multiplicadorBoss = case enemyClassSpec spec of
        BossAcelerador
          | percentagemVida <= 0.3 -> 2.1
          | percentagemVida <= 0.65 -> 1.55
        Elite | percentagemVida <= 0.35 -> 1.22
        _ -> 1
      velocidadeAtual = velocidadeInimigo inimigo
      velocidadeNova = if velocidadeAtual <= 0 then 0 else velocidadeAtual * multiplicadorBoss
   in inimigo {vidaInimigo = vidaNova, velocidadeInimigo = velocidadeNova}
