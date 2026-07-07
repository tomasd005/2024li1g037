-- |
-- Módulo      : Tarefa1
-- Descrição   : Invariantes do Jogo
-- Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
--               Ines Braga da Silva <a112819@alunos.uminho.pt>
--
-- Este módulo implementa funções para verificar as invariantes de um jogo, como a validade
-- dos portais, torres, inimigos e base.
module Tarefa1 where

import Data.List
import LI12425
import MapGeometry

mapa1 :: [[Terreno]]
mapa1 =
  [ [t, t, r, a, a, a],
    [r, t, r, a, r, r],
    [r, t, r, a, r, t],
    [r, t, r, a, r, t],
    [r, t, t, t, t, t],
    [a, a, a, a, r, r]
  ]
  where
    t = Terra
    r = Relva
    a = Agua

base1 :: Base
base1 = Base {posicaoBase = (1, 1), creditosBase = 100, vidaBase = 100.0}

portal1 :: Portal
portal1 = Portal {posicaoPortal = (0, 0), ondasPortal = []}

inimigo1 :: Inimigo
inimigo1 = Inimigo {posicaoInimigo = (0, 0), direcaoInimigo = Este, velocidadeInimigo = 1.0, vidaInimigo = 100.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = []}

torre1 :: Torre
torre1 = Torre {posicaoTorre = (0, 1), alcanceTorre = 2.0, rajadaTorre = 3, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 2.0, projetilTorre = Projetil Fogo (Finita 1.0)}

jogo1 :: Jogo
jogo1 = Jogo {mapaJogo = mapa1, baseJogo = base1, portaisJogo = [portal1], inimigosJogo = [inimigo1], torresJogo = [torre1], lojaJogo = [(50, torre1)]}

-- |
-- Verifica se o estado de um jogo é válido.
--
-- === Exemplo de utilização:
-- >>> validaJogo jogo1
-- True
validaJogo :: Jogo -> Bool
validaJogo jogo =
  all ($ jogo) [validaPortais, validaInimigos, validaTorres, validaBase]

-- |
-- Verifica se todos os portais de um jogo estão em posições válidas no mapa.
--
-- === Exemplo de utilização:
-- >>> validaPortais jogo1
-- True
validaPortais :: Jogo -> Bool
validaPortais jogo =
  let portais = portaisJogo jogo
      mapa = mapaJogo jogo
   in all (\portal -> posicaoPortalValida (posicaoPortal portal) mapa) portais

-- |
-- Verifica se uma posição de portal é válida no mapa.
--
-- Uma posição é válida se estiver dentro dos limites do mapa e o terreno correspondente não for "Água".
--
-- === Exemplo de utilização:
-- >>> posicaoPortalValida (0, 0) mapa1
-- True
posicaoPortalValida :: Posicao -> Mapa -> Bool
posicaoPortalValida pos mapa =
  case terrenoPorPosicao pos mapa of
    Just terra -> terra /= Agua
    Nothing -> False

-- |
-- Verifica se uma onda de inimigos associada a um portal é válida.
--
--
--
-- === Exemplo de utilização:
-- >>> validaOndaPortal portal1
-- True
validaOndaPortal :: Portal -> Bool
validaOndaPortal portal = all ondaSemInimigos (ondasPortal portal)

-- |
-- Verifica se uma onda está vazia (sem inimigos).
--
-- === Exemplo de utilização:
-- >>> ondaSemInimigos (Onda [] 1 1 1)
-- True
ondaSemInimigos :: Onda -> Bool
ondaSemInimigos onda = null (inimigosOnda onda)

-- |
-- Verifica se existe pelo menos um portal no jogo.
--
-- === Exemplo de utilização:
-- >>> minimoPortal [portal1]
-- True
minimoPortal :: [Portal] -> Bool
minimoPortal portais = not (null portais)

-- |
-- Obtém o terreno correspondente a uma posição no mapa.
--
-- === Exemplo de utilização:
-- >>> terrenoPorPosicao (0, 0) mapa1
-- Just Terra
terrenoPorPosicao :: Posicao -> Mapa -> Maybe Terreno
terrenoPorPosicao (x, y) mapa =
  terrenoEmCelula mapa (floor x) (floor y)

-- |
-- Verifica se todos os portais estão posicionados sobre terrenos "Terra" no mapa.
--
-- === Exemplo de utilização:
-- >>> posicionadoEmTerra mapa1 [portal1]
-- True
posicionadoEmTerra :: Mapa -> [Portal] -> Bool
posicionadoEmTerra mapa portais = all (\portal -> terrenoCaminhavel (posicaoPortal portal) mapa) portais

-- |
-- Verifica se todas as torres estão posicionadas sobre terrenos "Relva" no mapa.
--
-- === Exemplo de utilização:
-- >>> posicionadoEmRelvaTorre mapa1 [torre1]
-- True
posicionadoEmRelvaTorre :: Mapa -> [Torre] -> Bool
posicionadoEmRelvaTorre mapa torres = all (\torre -> terrenoPorPosicao (posicaoTorre torre) mapa == Just Relva) torres

-- |
-- Verifica se torres, portais e a base não estão sobrepostos.
--
-- === Exemplo de utilização:
-- >>> naoSobrepostosTorreBase [] base1 [torre1] [portal1] mapa1
-- True
naoSobrepostosTorreBase :: [Posicao] -> Base -> [Torre] -> [Portal] -> Mapa -> Bool
naoSobrepostosTorreBase _ base torres portais mapa =
  posicionadoEmRelvaTorre mapa torres
    && posicionadoEmTerraBase mapa base
    && verificaPosicaoTorreEmPortal mapa torres portais
    && verificaPosicaoBaseEmPortal mapa base portais

-- |
-- Verifica se uma torre não está sobre um portal.
--
-- === Exemplo de utilização:
-- >>> verificaPosicaoTorreEmPortal mapa1 [torre1] [portal1]
-- True
verificaPosicaoTorreEmPortal :: Mapa -> [Torre] -> [Portal] -> Bool
verificaPosicaoTorreEmPortal _ torres portais =
  all (\torre -> posicaoTorre torre `notElem` map posicaoPortal portais) torres

-- |
-- Verifica se a base não está sobre um portal.
--
-- === Exemplo de utilização:
-- >>> verificaPosicaoBaseEmPortal mapa1 base1 [portal1]
-- True
verificaPosicaoBaseEmPortal :: Mapa -> Base -> [Portal] -> Bool
verificaPosicaoBaseEmPortal _ base portais =
  posicaoBase base `notElem` map posicaoPortal portais

-- |
-- Verifica se há no máximo uma onda por portal.
--
-- === Exemplo de utilização:
-- >>> maximoOndaPorPortal [portal1]
-- True
maximoOndaPorPortal :: [Portal] -> Bool
maximoOndaPorPortal portais = all (\portal -> length (ondasPortal portal) <= 1) portais

-- |
-- Verifica se existe um caminho entre qualquer portal e a base no mapa.
--
-- === Exemplo de utilização:
-- >>> caminhoPortalBase mapa1 [(1, 1), (2, 2)] (0, 0)
-- True
caminhoPortalBase :: Mapa -> [Posicao] -> Posicao -> Bool
caminhoPortalBase mapa portais base = any (\portal -> buscaCaminho mapa portal base []) portais

-- |
-- Realiza a busca de um caminho entre uma posição atual e o destino no mapa.
--
--
-- === Exemplo de utilização:
-- >>> buscaCaminho mapa1 (1, 1) (0, 0) []
-- True
buscaCaminho :: Mapa -> Posicao -> Posicao -> [Posicao] -> Bool
buscaCaminho mapa atual destino visitados
  | atual == destino = True
  | not (posicaoValida mapa atual) = False
  | atual `elem` visitados = False
  | not (terrenoCaminhavel atual mapa) = False
  | otherwise =
      let visitados' = atual : visitados
          vizinhos = adjacentes atual
       in any (\pos -> buscaCaminho mapa pos destino visitados') vizinhos

-- |
-- Obtém as posições adjacentes a uma posição dada.
--
-- === Exemplo de utilização:
-- >>> adjacentes (1.0, 1.0)
-- [(0.0,1.0),(2.0,1.0),(1.0,0.0),(1.0,2.0)]
adjacentes :: Posicao -> [Posicao]
adjacentes (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- |
-- Verifica se uma posição é válida no mapa, considerando os limites.
--
-- === Exemplo de utilização:
-- >>> posicaoValida mapa1 (1.5, 2.5)
-- True
posicaoValida :: Mapa -> (Float, Float) -> Bool
posicaoValida mapa (x, y) =
  dentroMapa mapa (floor x) (floor y)

-- |
-- Verifica as condições de validade dos inimigos no jogo.
--
-- === Exemplo de utilização:
-- >>> validaInimigos jogo1
-- True
validaInimigos :: Jogo -> Bool
validaInimigos jogo = all validaInimigo (inimigosJogo jogo)
  where
    validaInimigo inimigo =
      vidaInimigo inimigo > 0
        && velocidadeInimigo inimigo >= 0
        && validaProjeteis (projeteisInimigo inimigo)
        && not (sobrepoeTorres inimigo (torresJogo jogo))

    validaProjeteis projeteis =
      let tipos = map tipoProjetil projeteis
       in length (nub tipos) == length tipos
            && not (incompatíveis tipos)

    incompatíveis tipos =
      (Fogo `elem` tipos && Resina `elem` tipos)
        || (Fogo `elem` tipos && Gelo `elem` tipos)

    -- Verifica se o inimigo está sobreposto a uma torre
    sobrepoeTorres inimigo torres =
      any (\torre -> posicaoTorre torre == posicaoInimigo inimigo) torres

-- |
-- Função que valida se todas as torres no jogo estão corretamente posicionadas e não têm conflitos.
--
--
-- === Exemplo de utilização:
-- >>> validaTorres jogo1
-- True
validaTorres :: Jogo -> Bool
validaTorres jogo =
  posicionadoEmRelvaTorre (mapaJogo jogo) (torresJogo jogo)
    && naoSobrepostosTorreBase (map posicaoTorre (torresJogo jogo)) (baseJogo jogo) (torresJogo jogo) (portaisJogo jogo) (mapaJogo jogo)
    && all (\torre -> alcanceTorre torre > 0) (torresJogo jogo)

-- |
-- Função que verifica se todas as torres estão posicionadas sobre o terreno de relva.
--
--
-- === Exemplo de utilização:
-- >>> todasEmRelva (mapaJogo jogo1) (torresJogo jogo1)
-- True
todasEmRelva :: Mapa -> [Torre] -> Bool
todasEmRelva mapa = all (\torre -> terrenoPorPosicao (posicaoTorre torre) mapa == Just Relva)

-- |
-- Função que verifica se todas as torres têm um alcance positivo.
--
--
-- === Exemplo de utilização:
-- >>> alcancesPositivos (torresJogo jogo1)
-- True
alcancesPositivos :: [Torre] -> Bool
alcancesPositivos = all (\torre -> alcanceTorre torre > 0)

-- |
-- Função que verifica se todas as torres têm rajadas de tiro positivas.
--
--
-- === Exemplo de utilização:
-- >>> rajadasPositivas (torresJogo jogo1)
-- True
rajadasPositivas :: [Torre] -> Bool
rajadasPositivas = all (\torre -> rajadaTorre torre > 0)

-- |
-- Função que verifica se todas as torres têm ciclos de rajada não negativos.
--
--
-- === Exemplo de utilização:
-- >>> ciclosNaoNegativos (torresJogo jogo1)
-- True
ciclosNaoNegativos :: [Torre] -> Bool
ciclosNaoNegativos = all (\torre -> cicloTorre torre >= 0)

-- |
-- Função que verifica se não há sobreposição de torres.
--
--
-- === Exemplo de utilização:
-- >>> naoSobrepostas (torresJogo jogo1)
-- True
naoSobrepostas :: [Torre] -> Bool
naoSobrepostas torres =
  let posicoes = map posicaoTorre torres
   in length posicoes == length (nub posicoes)

-- |
-- Função que valida o estado da base do jogo.
--
--
-- === Exemplo de utilização:
-- >>> validaBase jogo1
-- True
validaBase :: Jogo -> Bool
validaBase jogo =
  baseSobreTerra (mapaJogo jogo) (baseJogo jogo)
    && creditosBase (baseJogo jogo) >= 0
    && not (sobrepoeTorreOuPortal (baseJogo jogo) (torresJogo jogo) (portaisJogo jogo))
  where
    baseSobreTerra mapa base =
      case terrenoPorPosicao (posicaoBase base) mapa of
        Just Terra -> True
        Just Asfalto -> True
        _ -> False

    sobrepoeTorreOuPortal base torres portais =
      let posBase = posicaoBase base
       in any (\torre -> posicaoTorre torre == posBase) torres
            || any (\portal -> posicaoPortal portal == posBase) portais

-- |
-- Função que verifica se a base está posicionada sobre um terreno de terra.
--
--
-- === Exemplo de utilização:
-- >>> posicionadoEmTerraBase (mapaJogo jogo1) (baseJogo jogo1)
-- True
posicionadoEmTerraBase :: Mapa -> Base -> Bool
posicionadoEmTerraBase mapa base =
  terrenoCaminhavel (posicaoBase base) mapa

terrenoCaminhavel :: Posicao -> Mapa -> Bool
terrenoCaminhavel pos mapa =
  case terrenoPorPosicao pos mapa of
    Just Terra -> True
    Just Asfalto -> True
    _ -> False
