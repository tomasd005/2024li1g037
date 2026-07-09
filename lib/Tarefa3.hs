-- |
-- Module      : Tarefa3
-- Description : Mecânica do Jogo
-- Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
--               Ines Braga da Silva <a112819@alunos.uminho.pt>
--
-- Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-- Implementa toda a mecânica de jogo incluindo movimento de inimigos,
-- disparo de torres, e atualização do estado do jogo.
module Tarefa3 where

import Data.Function (on)
import Data.List (sortBy)
import qualified Data.IntMap.Strict as IntMap
import EnemySpatial
import LI12425
import MapGeometry
import Tarefa2

-- ============================================================================
-- FUNÇÃO PRINCIPAL - Atualiza estado completo do jogo
-- ============================================================================

-- | A função 'atualizaJogo' atualiza o estado completo do jogo dado um intervalo de tempo.
--
-- Esta é a função principal da Tarefa 3 que coordena todas as atualizações do jogo:
-- 1. Atualiza portais e lança inimigos
-- 2. Atualiza torres e dispara projéteis
-- 3. Move inimigos e aplica efeitos
-- 4. Remove inimigos mortos e os que chegaram à base
-- 5. Atualiza créditos e vida da base
--
-- == Exemplo:
-- >>> let jogo' = atualizaJogo 1.0 jogo1
-- >>> vidaBase (baseJogo jogo') <= vidaBase (baseJogo jogo1)
-- True
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo tempo jogo =
  let -- 1. Atualizar portais e lançar inimigos
      (portaisAtualizados, inimigosLancados) =
        atualizaTodosPortais tempo (portaisJogo jogo)
      
      todosInimigos = inimigosJogo jogo ++ inimigosLancados
      
      -- 2. Atualizar torres e disparar
      (torresAtualizadas, inimigosAposDisparo) =
        atualizaTodasTorres tempo todosInimigos (torresJogo jogo)
      
      -- 3. Remover inimigos que já chegaram à base antes de os mover.
      -- Sem esta separação, um inimigo que começa o ciclo em cima da base pode
      -- mover-se para fora dela antes de causar dano.
      (inimigosMortosAntesMovimento, inimigosParaMover, inimigosNaBaseAntesMovimento) =
        separaInimigosPorEstado inimigosAposDisparo (posicaoBase (baseJogo jogo))

      -- 4. Mover inimigos restantes e aplicar efeitos
      inimigosMovidos =
        map (atualizaInimigo tempo (mapaJogo jogo)) inimigosParaMover
      
      -- 5. Separar inimigos mortos, vivos e que chegaram à base depois de mover
      (inimigosMortosDepoisMovimento, inimigosVivos, inimigosNaBaseDepoisMovimento) =
        separaInimigosPorEstado inimigosMovidos (posicaoBase (baseJogo jogo))
      
      inimigosMortos = inimigosMortosAntesMovimento ++ inimigosMortosDepoisMovimento
      inimigosNaBase = inimigosNaBaseAntesMovimento ++ inimigosNaBaseDepoisMovimento

      -- 6. Calcular butim e dano
      butimGanho = sum (map butimInimigo inimigosMortos)
      danoSofrido = sum (map ataqueInimigo inimigosNaBase)
      
      -- 7. Atualizar base
      baseAtualizada = (baseJogo jogo) {
        creditosBase = max 0 (creditosBase (baseJogo jogo) + butimGanho),
        vidaBase = max 0 (vidaBase (baseJogo jogo) - danoSofrido)
      }
  in jogo {
    torresJogo = torresAtualizadas,
    inimigosJogo = inimigosVivos,
    portaisJogo = portaisAtualizados,
    baseJogo = baseAtualizada
  }

-- ============================================================================
-- ATUALIZAÇÃO DE PORTAIS E ONDAS
-- ============================================================================

-- | Atualiza todos os portais do jogo e retorna os inimigos lançados.
--
-- == Exemplo:
-- >>> let (portais', inimigos') = atualizaTodosPortais 1.0 [portal01]
-- >>> length inimigos' >= 0
-- True
atualizaTodosPortais :: Tempo -> [Portal] -> ([Portal], [Inimigo])
atualizaTodosPortais tempo portais =
  foldr (\portal (ps, is) -> 
    let (p', i') = atualizaPortalELanca tempo portal
     in (p':ps, i' ++ is)
  ) ([], []) portais

-- | Atualiza um portal e lança inimigos se necessário.
--
-- == Exemplo:
-- >>> let (portal', inimigos') = atualizaPortalELanca 1.0 portal01
-- >>> length (ondasPortal portal') <= length (ondasPortal portal01)
-- True
atualizaPortalELanca :: Tempo -> Portal -> (Portal, [Inimigo])
atualizaPortalELanca tempo portal =
  let (ondasAtualizadas, inimigosLancados) = 
        processaOndas tempo (ondasPortal portal) (posicaoPortal portal)
   in (portal {ondasPortal = ondasAtualizadas}, inimigosLancados)

-- | Processa todas as ondas de um portal.
--
-- == Exemplo:
-- >>> let (ondas', inimigos') = processaOndas 1.0 [onda01] (0, 2)
-- >>> length ondas' <= 1
-- True
processaOndas :: Tempo -> [Onda] -> Posicao -> ([Onda], [Inimigo])
processaOndas _ [] _ = ([], [])
processaOndas tempo (onda:ondas) posPortal
  -- Se a onda ainda não começou, decrementa entradaOnda
  | entradaOnda onda > 0 =
      let ondaAtualizada = onda {entradaOnda = max 0 (entradaOnda onda - tempo)}
       in (ondaAtualizada:ondas, [])
  -- Se ainda não pode lançar o próximo inimigo, decrementa tempoOnda
  | tempoOnda onda > 0 =
      let ondaAtualizada = onda {tempoOnda = max 0 (tempoOnda onda - tempo)}
       in (ondaAtualizada:ondas, [])
  -- Se não há mais inimigos nesta onda, passa para a próxima
  | null (inimigosOnda onda) =
      -- Processa as próximas ondas recursivamente
      processaOndas tempo ondas posPortal
  -- Caso contrário, lança um inimigo
  | inimigo : restantes <- inimigosOnda onda =
      let inimigoLancado = inimigo {posicaoInimigo = posPortal}
          -- Se ainda há inimigos, mantém a onda com tempo resetado
          -- Se não há mais, remove a onda
          novasOndas = if null restantes
                       then ondas
                       else onda {inimigosOnda = restantes, tempoOnda = cicloOnda onda} : ondas
       in (novasOndas, [inimigoLancado])
  | otherwise = processaOndas tempo ondas posPortal

-- ============================================================================
-- ATUALIZAÇÃO DE TORRES
-- ============================================================================

-- | Atualiza todas as torres do jogo.
--
-- == Exemplo:
-- >>> let (torres', inimigos') = atualizaTodasTorres 1.0 [inimigo1] [torre1]
-- >>> length torres' == 1
-- True
atualizaTodasTorres :: Tempo -> [Inimigo] -> [Torre] -> ([Torre], [Inimigo])
atualizaTodasTorres tempo inimigos torres =
  let inimigosIndexados = zip [0 :: Int ..] inimigos
      alcanceMaximo = foldr (max . alcanceTorre) 1 torres
      grid = buildSpatialGrid alcanceMaximo inimigosIndexados
      inimigosPorIndice = IntMap.fromAscList inimigosIndexados
      (torresAtualizadas, inimigosAtualizados) =
        foldr (atualizaTorreComGrid tempo grid) ([], inimigosPorIndice) torres
   in (torresAtualizadas, IntMap.elems inimigosAtualizados)

atualizaTorreComGrid :: Tempo -> SpatialGrid -> Torre -> ([Torre], IntMap.IntMap Inimigo) -> ([Torre], IntMap.IntMap Inimigo)
atualizaTorreComGrid tempo grid torre (torresAtualizadas, inimigosPorIndice) =
  let torreComTempoAtualizado = torre {tempoTorre = tempoTorre torre - tempo}
   in if tempoTorre torreComTempoAtualizado <= 0
      then
        let (torreRecarregada, inimigosAposDisparo) =
              dispararTorreComGrid grid torreComTempoAtualizado inimigosPorIndice
         in (torreRecarregada : torresAtualizadas, inimigosAposDisparo)
      else (torreComTempoAtualizado : torresAtualizadas, inimigosPorIndice)

dispararTorreComGrid :: SpatialGrid -> Torre -> IntMap.IntMap Inimigo -> (Torre, IntMap.IntMap Inimigo)
dispararTorreComGrid grid torre inimigosPorIndice =
  let candidatos = inimigosNoAlcanceGrid grid torre inimigosPorIndice
      inimigosOrdenados = sortBy (compare `on` distanciaDaTorreIndice torre) candidatos
      alvos = take (rajadaTorre torre) inimigosOrdenados
      inimigosAtingidos =
        foldl' (\acc (indice, inimigo) -> IntMap.insert indice (atingeInimigo torre inimigo) acc) inimigosPorIndice alvos
      torreRecarregada = torre {tempoTorre = cicloTorre torre}
   in (torreRecarregada, inimigosAtingidos)

inimigosNoAlcanceGrid :: SpatialGrid -> Torre -> IntMap.IntMap Inimigo -> [(Int, Inimigo)]
inimigosNoAlcanceGrid grid torre inimigosPorIndice =
  let alcance = alcanceTorre torre
      alcanceQuadrado = alcance * alcance
      posTorre = posicaoTorre torre
      candidatos = queryEnemyIndices grid posTorre alcance
   in foldr (adicionaSeDentro alcanceQuadrado posTorre) [] candidatos
  where
    adicionaSeDentro alcanceQuadrado posTorre indice acc =
      case IntMap.lookup indice inimigosPorIndice of
        Just inimigo
          | distanciaQuadrada posTorre (posicaoInimigo inimigo) <= alcanceQuadrado ->
              (indice, inimigo) : acc
        _ -> acc

distanciaDaTorreIndice :: Torre -> (Int, Inimigo) -> Float
distanciaDaTorreIndice torre (_, inimigo) = distanciaDaTorre torre inimigo

-- | Atualiza uma torre: decrementa tempo e dispara se pronta.
--
-- == Exemplo:
-- >>> let (torre', _) = atualizaTorre 1.0 [inimigo1] torre1
-- >>> tempoTorre torre' <= tempoTorre torre1
-- True
atualizaTorre :: Tempo -> [Inimigo] -> Torre -> (Torre, [Inimigo])
atualizaTorre tempo inimigos torre =
  let torreComTempoAtualizado = torre {tempoTorre = tempoTorre torre - tempo}
   in if tempoTorre torreComTempoAtualizado <= 0
      then dispararTorre torreComTempoAtualizado inimigos
      else (torreComTempoAtualizado, inimigos)

-- | Torre dispara contra inimigos ao alcance.
--
-- == Exemplo:
-- >>> let (torre', inimigos') = dispararTorre torre1 [inimigo1]
-- >>> tempoTorre torre' == cicloTorre torre1
-- True
dispararTorre :: Torre -> [Inimigo] -> (Torre, [Inimigo])
dispararTorre torre inimigos =
  let inimigosAlcance = inimigosNoAlcance torre inimigos
      inimigosOrdenados = sortBy (compare `on` distanciaDaTorre torre) inimigosAlcance
      inimigosParaAtingir = take (rajadaTorre torre) inimigosOrdenados
      inimigosAtingidos = map (atingeInimigo torre) inimigosParaAtingir
      posicoesAtingidas = map posicaoInimigo inimigosParaAtingir
      outrosInimigos = filter (\i -> posicaoInimigo i `notElem` posicoesAtingidas) inimigos
      torreRecarregada = torre {tempoTorre = cicloTorre torre}
   in (torreRecarregada, inimigosAtingidos ++ outrosInimigos)

-- | Calcula distância de inimigo à torre.
--
-- == Exemplo:
-- >>> distanciaDaTorre torre1 inimigo1 >= 0
-- True
distanciaDaTorre :: Torre -> Inimigo -> Float
distanciaDaTorre torre inimigo =
  let (x1, y1) = posicaoTorre torre
      (x2, y2) = posicaoInimigo inimigo
   in distanciaQuadrada (x1, y1) (x2, y2)

-- ============================================================================
-- ATUALIZAÇÃO DE INIMIGOS
-- ============================================================================

-- | Atualiza todos os inimigos do jogo.
--
-- == Exemplo:
-- >>> let inimigos' = atualizaInimigos 1.0 mapa1 [inimigo1]
-- >>> length inimigos' >= 0
-- True
atualizaInimigos :: Tempo -> Mapa -> [Inimigo] -> [Inimigo]
atualizaInimigos tempo mapa = map (atualizaInimigo tempo mapa)

-- | Atualização completa de um inimigo.
--
-- == Exemplo:
-- >>> let inimigo' = atualizaInimigo 1.0 mapa1 inimigo1
-- >>> vidaInimigo inimigo' <= vidaInimigo inimigo1
-- True
atualizaInimigo :: Tempo -> Mapa -> Inimigo -> Inimigo
atualizaInimigo tempo mapa inimigo =
  inimigo
    & atualizaProjetis tempo
    & aplicaEfeitoProjetil tempo
    & moverInimigoSePermitido tempo mapa
    & removeProjeteisExpirados
  where
    (&) = flip ($)

-- | Atualiza durações de todos os projéteis.
--
-- == Exemplo:
-- >>> let projs' = atualizaProjetis 1.0 [projetil1]
-- >>> length projs' >= 0
-- True
atualizaProjetis :: Tempo -> Inimigo -> Inimigo
atualizaProjetis tempo inimigo =
  inimigo {projeteisInimigo = map (atualizaDuracaoProjetil tempo) (projeteisInimigo inimigo)}

-- | Atualiza a duração de um projétil.
--
-- == Exemplo:
-- >>> atualizaDuracaoProjetil 1.0 projetil1
-- Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 4.0}
atualizaDuracaoProjetil :: Tempo -> Projetil -> Projetil
atualizaDuracaoProjetil tempo projetil =
  projetil {duracaoProjetil = atualizaDuracao (duracaoProjetil projetil) tempo}

-- | Atualiza uma duração.
--
-- == Exemplo:
-- >>> atualizaDuracao (Finita 5.0) 1.0
-- Finita 4.0
atualizaDuracao :: Duracao -> Tempo -> Duracao
atualizaDuracao Infinita _ = Infinita
atualizaDuracao (Finita d) t = Finita (max 0 (d - t))

-- | Aplica os efeitos dos projéteis ativos no inimigo.
--
-- == Exemplo:
-- >>> let inimigo' = aplicaEfeitoProjetil 1.0 inimigo2
-- >>> vidaInimigo inimigo' <= vidaInimigo inimigo2
-- True
aplicaEfeitoProjetil :: Tempo -> Inimigo -> Inimigo
aplicaEfeitoProjetil tempo inimigo =
  let projeteis = projeteisInimigo inimigo
      velocidadeBase = velocidadeBaseInimigo inimigo
      -- Efeito de fogo: 2 dano por segundo
      temFogo = any (\p -> tipoProjetil p == Fogo) projeteis
      temVeneno = any (\p -> tipoProjetil p == Veneno) projeteis
      danoContinuo = (if temFogo then 2.0 else 0) + (if temVeneno then 3.2 else 0)
      vidaAposEfeitos = max 0 (vidaInimigo inimigo - danoContinuo * tempo)
      
      -- Efeito de gelo: impede movimento
      temGelo = any (\p -> tipoProjetil p == Gelo) projeteis
      temEletrico = any (\p -> tipoProjetil p == Eletrico) projeteis
      controloTotal = temGelo || temEletrico
      velocAposControlo = if controloTotal then 0 else velocidadeBase
      
      -- Efeito de resina: reduz velocidade para 80%
      temResina = any (\p -> tipoProjetil p == Resina) projeteis
      velocFinal
        | controloTotal = 0
        | temResina = velocidadeBase * 0.8
        | otherwise = velocAposControlo
   
   in inimigo {
        vidaInimigo = vidaAposEfeitos,
        velocidadeInimigo = velocFinal
      }

-- | Move inimigo se velocidade > 0.
--
-- == Exemplo:
-- >>> let inimigo' = moverInimigoSePermitido 1.0 mapa1 inimigo1
-- >>> posicaoInimigo inimigo' /= posicaoInimigo inimigo1 || velocidadeInimigo inimigo1 == 0
-- True
moverInimigoSePermitido :: Tempo -> Mapa -> Inimigo -> Inimigo
moverInimigoSePermitido tempo mapa inimigo =
  if velocidadeInimigo inimigo > 0
  then moverInimigo tempo mapa inimigo
  else inimigo

-- | Remove projéteis que já expiraram.
--
-- == Exemplo:
-- >>> let inimigo' = removeProjeteisExpirados inimigo1
-- >>> length (projeteisInimigo inimigo') <= length (projeteisInimigo inimigo1)
-- True
removeProjeteisExpirados :: Inimigo -> Inimigo
removeProjeteisExpirados inimigo =
  let projeteisValidos = filter projetilValido (projeteisInimigo inimigo)
   in inimigo {projeteisInimigo = projeteisValidos}
  where
    projetilValido p = case duracaoProjetil p of
      Infinita -> True
      Finita t -> t > 0

-- ============================================================================
-- MOVIMENTO DE INIMIGOS
-- ============================================================================

-- | Move inimigo no mapa seguindo caminho de terra.
--
-- == Exemplo:
-- >>> let inimigo' = moverInimigo 1.0 mapa1 inimigo1
-- >>> posicaoInimigo inimigo' /= posicaoInimigo inimigo1 || velocidadeInimigo inimigo1 == 0
-- True
moverInimigo :: Tempo -> Mapa -> Inimigo -> Inimigo
moverInimigo tempo mapa inimigo =
  let assustado = any (\p -> tipoProjetil p == Medo) (projeteisInimigo inimigo)
      novaDirecao = if assustado
                    then getDirecaoOposta (direcaoInimigo inimigo)
                    else proximaDirecao mapa (posicaoInimigo inimigo) (direcaoInimigo inimigo)
      distancia = velocidadeInimigo inimigo * tempo * multiplicadorTerreno mapa (posicaoInimigo inimigo)
      novaPos = moverNaDirecao (posicaoInimigo inimigo) novaDirecao distancia
   in inimigo {posicaoInimigo = novaPos, direcaoInimigo = novaDirecao}

-- | Calcula nova posição baseada na direção e distância.
--
-- == Exemplo:
-- >>> moverNaDirecao (1.0, 1.0) Este 1.0
-- (2.0,1.0)
moverNaDirecao :: Posicao -> Direcao -> Float -> Posicao
moverNaDirecao (x, y) direcao dist = case direcao of
  Norte -> (x, y - dist)
  Sul   -> (x, y + dist)
  Este  -> (x + dist, y)
  Oeste -> (x - dist, y)

-- | Determina próxima direção válida (sem voltar atrás).
--
-- == Exemplo:
-- >>> proximaDirecao mapa1 (1, 1) Este
-- Este
proximaDirecao :: Mapa -> Posicao -> Direcao -> Direcao
proximaDirecao mapa pos direcaoAtual =
  let vizinhos = vizinhosValidos mapa pos
      direcaoOposta = getDirecaoOposta direcaoAtual
      direcoesPermitidas = filter (\(d, _) -> d /= direcaoOposta) vizinhos
   in case direcoesPermitidas of
        [] -> direcaoAtual
        primeiraDirecao : _ ->
          case filter (\(d, _) -> d == direcaoAtual) direcoesPermitidas of
            (d, _) : _ -> d
            [] -> fst primeiraDirecao

-- | Retorna vizinhos válidos (em terra).
--
-- == Exemplo:
-- >>> length (vizinhosValidos mapa1 (1, 1)) >= 0
-- True
vizinhosValidos :: Mapa -> Posicao -> [(Direcao, Posicao)]
vizinhosValidos mapa (x, y) =
  let possibilidades = [
        (Norte, (x, y - 1)),
        (Sul, (x, y + 1)),
        (Este, (x + 1, y)),
        (Oeste, (x - 1, y))
        ]
   in filter (\(_, pos) -> posicaoValidaTerra mapa pos) possibilidades

-- | Verifica se posição é válida e em terra.
--
-- == Exemplo:
-- >>> posicaoValidaTerra mapa1 (0, 0)
-- True
posicaoValidaTerra :: Mapa -> Posicao -> Bool
posicaoValidaTerra mapa (x, y) =
  terrenoEmCelula mapa (floor x) (floor y) `elem` [Just Terra, Just Asfalto]

multiplicadorTerreno :: Mapa -> Posicao -> Float
multiplicadorTerreno mapa (x, y) =
  case terrenoEmCelula mapa (floor x) (floor y) of
    Just Asfalto -> 1.45
    _ -> 1

-- | Retorna direção oposta.
--
-- == Exemplo:
-- >>> getDirecaoOposta Norte
-- Sul
getDirecaoOposta :: Direcao -> Direcao
getDirecaoOposta Norte = Sul
getDirecaoOposta Sul = Norte
getDirecaoOposta Este = Oeste
getDirecaoOposta Oeste = Este

-- ============================================================================
-- FUNÇÕES AUXILIARES
-- ============================================================================

-- | Separa inimigos em três categorias: mortos, vivos e que chegaram à base.
--
-- == Exemplo:
-- >>> let (mortos, vivos, naBase) = separaInimigosPorEstado [inimigo1] (1, 1)
-- >>> length mortos + length vivos + length naBase == 1
-- True
separaInimigosPorEstado :: [Inimigo] -> Posicao -> ([Inimigo], [Inimigo], [Inimigo])
separaInimigosPorEstado inimigos posBase =
  foldr classificar ([], [], []) inimigos
  where
    classificar i (mortos, vivos, naBase)
      | vidaInimigo i <= 0 = (i:mortos, vivos, naBase)
      | posicaoInimigo i `proxima` posBase = (mortos, vivos, i:naBase)
      | otherwise = (mortos, i:vivos, naBase)
    
    proxima p1 p2 =
      distanciaQuadrada p1 p2 < 0.25

-- | Verifica se inimigo morreu.
--
-- == Exemplo:
-- >>> inimigoMorreu inimigo1
-- False
inimigoMorreu :: Inimigo -> Bool
inimigoMorreu inimigo = vidaInimigo inimigo <= 0

-- | Cria um projétil de uma torre.
--
-- == Exemplo:
-- >>> tipoProjetil (criarProjetil torre2 inimigo1)
-- Fogo
criarProjetil :: Torre -> Inimigo -> Projetil
criarProjetil torre _ = projetilTorre torre

-- | Aplica efeito de um projétil a um inimigo.
--
-- == Exemplo:
-- >>> let inimigo' = aplicaEfeito projetil1 inimigo1
-- >>> length (projeteisInimigo inimigo') >= 1
-- True
aplicaEfeito :: Projetil -> Inimigo -> Inimigo
aplicaEfeito projetil inimigo =
  let projeteisAtualizados = atualizarProjetis projetil (projeteisInimigo inimigo)
   in inimigo {projeteisInimigo = projeteisAtualizados}

-- | Verifica se um inimigo atingiu a base.
--
-- == Exemplo:
-- >>> atingiuBase base1 inimigo1
-- False
atingiuBase :: Base -> Inimigo -> Bool
atingiuBase base inimigo =
  distanciaQuadrada (posicaoBase base) (posicaoInimigo inimigo) < 0.25

-- | Atualiza a base do jogo.
--
-- == Exemplo:
-- >>> let base' = atualizaBase 1.0 base1
-- >>> vidaBase base' == vidaBase base1
-- True
atualizaBase :: Tempo -> Base -> Base
atualizaBase _ base = base

-- | Atualiza uma onda de inimigos.
--
-- == Exemplo:
-- >>> let onda' = atualizaOnda 1.0 [] onda01
-- >>> entradaOnda onda' <= entradaOnda onda01
-- True
atualizaOnda :: Tempo -> [Inimigo] -> Onda -> Onda
atualizaOnda tempo _ onda =
  onda {
    tempoOnda = max 0 (tempoOnda onda - tempo),
    entradaOnda = max 0 (entradaOnda onda - tempo)
  }

-- | Atualiza um portal.
--
-- == Exemplo:
-- >>> let portal' = atualizaPortal 1.0 [] portal01
-- >>> length (ondasPortal portal') <= length (ondasPortal portal01)
-- True
atualizaPortal :: Tempo -> [Inimigo] -> Portal -> Portal
atualizaPortal tempo _ portal =
  let (ondas', _) = processaOndas tempo (ondasPortal portal) (posicaoPortal portal)
   in portal {ondasPortal = ondas'}

distanciaQuadrada :: Posicao -> Posicao -> Float
distanciaQuadrada (x1, y1) (x2, y2) =
  let dx = x1 - x2
      dy = y1 - y2
   in dx * dx + dy * dy
