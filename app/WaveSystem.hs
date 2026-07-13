module WaveSystem
  ( EnemyGroup (..),
    WavePlan (..),
    InfiniteMutator (..),
    criaInimigo,
    criaOnda,
    criaOndaPlano,
    composicaoNivel,
    mutadoresOndaInfinita,
    aplicaMutadoresInfinito,
    nomeMutadores,
    ondasParaModo,
    onda01,
    onda02,
    onda03,
  )
where

import EnemySystem
import ImmutableTowers (ModoJogoEscolhido (..))
import LI12425

data EnemyGroup = EnemyGroup
  { classeGrupo :: !EnemyClass,
    quantidadeGrupo :: !Int
  }
  deriving (Show, Read, Eq)

data WavePlan = WavePlan
  { gruposPlano :: [EnemyGroup],
    cicloPlano :: !Tempo
  }
  deriving (Show, Read, Eq)

data InfiniteMutator
  = Fortificados
  | RecompensaEscassa
  | OndaDupla
  deriving (Show, Read, Eq)

onda01, onda02, onda03 :: Onda
onda01 = criaOnda 1 4 0
onda02 = criaOnda 2 5 4
onda03 = criaOnda 3 7 8

criaOnda :: Int -> Int -> Tempo -> Onda
criaOnda nivel quantidade entrada =
  criaOndaPlano nivel entrada (WavePlan (composicaoNivel nivel quantidade) (max 0.72 (2.7 - fromIntegral nivel * 0.08)))

criaOndaPlano :: Int -> Tempo -> WavePlan -> Onda
criaOndaPlano nivel entrada plano =
  Onda
    { inimigosOnda = concatMap criaGrupo (gruposPlano plano),
      cicloOnda = max 0.35 (cicloPlano plano),
      tempoOnda = 0,
      entradaOnda = max 0 entrada
    }
  where
    criaGrupo grupo = replicate (max 0 (quantidadeGrupo grupo)) (criaInimigoClasse (classeGrupo grupo) nivel)

criaInimigo :: Int -> Int -> Inimigo
criaInimigo nivel indice = criaInimigoClasse (classePorIndice nivel indice) nivel

classePorIndice :: Int -> Int -> EnemyClass
classePorIndice nivel indice
  | nivel >= 9 && indice `mod` 11 == 0 = Elite
  | nivel >= 7 && indice `mod` 9 == 0 = Protegido
  | nivel >= 6 && indice `mod` 8 == 0 = Dispersor
  | nivel >= 5 && indice `mod` 7 == 0 = Regenerador
  | nivel >= 4 && indice `mod` 6 == 0 = Blindado
  | indice `mod` 5 == 0 = Tanque
  | indice `mod` 4 == 0 = Rapido
  | otherwise = Basico

composicaoNivel :: Int -> Int -> [EnemyGroup]
composicaoNivel nivel quantidade =
  compacta
    [ EnemyGroup Basico (quantidade - rapidos - tanques - especiais),
      EnemyGroup Rapido rapidos,
      EnemyGroup Tanque tanques,
      EnemyGroup (especialNivel nivel) especiais
    ]
  where
    rapidos = if nivel >= 2 then max 1 (quantidade `div` 4) else 0
    tanques = if nivel >= 3 then max 1 (quantidade `div` 6) else 0
    especiais = if nivel >= 4 then max 1 (quantidade `div` 8) else 0

especialNivel :: Int -> EnemyClass
especialNivel nivel = case nivel `mod` 5 of
  0 -> Blindado
  1 -> Regenerador
  2 -> Dispersor
  3 -> Protegido
  _ -> Elite

compacta :: [EnemyGroup] -> [EnemyGroup]
compacta = filter ((> 0) . quantidadeGrupo)

mutadoresOndaInfinita :: Int -> [InfiniteMutator]
mutadoresOndaInfinita numeroOnda =
  [Fortificados | numeroOnda > 0 && numeroOnda `mod` 4 == 0]
    ++ [RecompensaEscassa | numeroOnda > 0 && numeroOnda `mod` 6 == 0]
    ++ [OndaDupla | numeroOnda > 0 && numeroOnda `mod` 9 == 0]

aplicaMutadoresInfinito :: Int -> Onda -> Onda
aplicaMutadoresInfinito numeroOnda onda =
  foldl aplica onda (mutadoresOndaInfinita numeroOnda)
  where
    aplica ondaAtual mutator = case mutator of
      Fortificados -> ondaAtual {inimigosOnda = map fortalece (inimigosOnda ondaAtual)}
      RecompensaEscassa -> ondaAtual {inimigosOnda = map reduzButim (inimigosOnda ondaAtual)}
      OndaDupla -> ondaAtual {inimigosOnda = inimigosOnda ondaAtual ++ inimigosOnda ondaAtual, cicloOnda = max 0.35 (cicloOnda ondaAtual * 0.82)}
    fortalece inimigo =
      inimigo
        { vidaInimigo = vidaInimigo inimigo * 1.20,
          ataqueInimigo = ataqueInimigo inimigo * 1.12
        }
    reduzButim inimigo = inimigo {butimInimigo = max 1 (floor (fromIntegral (butimInimigo inimigo) * (0.72 :: Float)))}

nomeMutadores :: [InfiniteMutator] -> String
nomeMutadores [] = "SEM MUTADOR"
nomeMutadores mutadores = concatCom " + " (map nome mutadores)
  where
    nome Fortificados = "FORTIFICADOS"
    nome RecompensaEscassa = "BUTIM REDUZIDO"
    nome OndaDupla = "ONDA DUPLA"

concatCom :: String -> [String] -> String
concatCom _ [] = ""
concatCom _ [texto] = texto
concatCom separador (texto : textos) = texto ++ separador ++ concatCom separador textos

ondasParaModo :: ModoJogoEscolhido -> [Onda]
ondasParaModo modoAtual = case modoAtual of
  ModoHistoria ->
    [criaOnda nivel (3 + nivel) (fromIntegral (nivel - 1) * 3.2) | nivel <- [1 .. 4]]
  ModoInfinito -> [criaOnda 1 5 0]
  ModoDesafio ->
    [ criaOndaPlano 4 0 (WavePlan [EnemyGroup Rapido 7, EnemyGroup Tanque 2] 0.72),
      criaOndaPlano 6 3 (WavePlan [EnemyGroup Blindado 4, EnemyGroup Dispersor 4] 0.9),
      criaOndaPlano 8 6 (WavePlan [EnemyGroup Protegido 5, EnemyGroup Regenerador 4, EnemyGroup Elite 1] 0.82)
    ]
  ModoBoss ->
    [ criaOndaPlano 6 0 (WavePlan [EnemyGroup Rapido 5, EnemyGroup BossAcelerador 1] 1.0),
      criaOndaPlano 8 5 (WavePlan [EnemyGroup Blindado 4, EnemyGroup BossGuardiao 1] 1.2),
      criaOndaPlano 10 10 (WavePlan [EnemyGroup Dispersor 4, EnemyGroup Regenerador 3, EnemyGroup BossRuptura 1] 1.05)
    ]
  ModoSandbox ->
    [criaOndaPlano 5 0 (WavePlan [EnemyGroup enemyClass 1 | enemyClass <- [Basico .. Elite]] 0.75)]
