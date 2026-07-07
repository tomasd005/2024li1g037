module MapData
  ( mapa01,
    mapa02,
    mapa03,
    mapa04,
    mapa05,
    mapaPorId,
    base01,
    basePorMapa,
    portalBase,
    portalPorMapa,
  )
where

import LI12425
import MetaTypes

larguraMapaPadrao, alturaMapaPadrao :: Int
larguraMapaPadrao = 36
alturaMapaPadrao = 34

mapa01, mapa02, mapa03, mapa04, mapa05 :: [[Terreno]]
mapa01 = criaMapaPlanicie
mapa02 = criaMapaGarganta
mapa03 = criaMapaLago
mapa04 = criaMapaCruzamento
mapa05 = criaMapaBastiao

mapaPorId :: MapId -> [[Terreno]]
mapaPorId mapaId = case mapaId of
  PlanicieSerena -> mapa01
  GargantaPedra -> mapa02
  LagoFraturado -> mapa03
  CruzamentoSolar -> mapa04
  BastiaoEspiral -> mapa05

base01 :: Base
base01 = basePorMapa PlanicieSerena

basePorMapa :: MapId -> Base
basePorMapa mapaId = case mapaId of
  PlanicieSerena -> Base {posicaoBase = (35.5, 22.5), creditosBase = 150, vidaBase = 80}
  GargantaPedra -> Base {posicaoBase = (31.5, 27.5), creditosBase = 150, vidaBase = 80}
  LagoFraturado -> Base {posicaoBase = (34.5, 24.5), creditosBase = 150, vidaBase = 80}
  CruzamentoSolar -> Base {posicaoBase = (4.5, 29.5), creditosBase = 150, vidaBase = 80}
  BastiaoEspiral -> Base {posicaoBase = (18.5, 28.5), creditosBase = 150, vidaBase = 80}

portalBase :: Portal
portalBase = portalPorMapa PlanicieSerena

portalPorMapa :: MapId -> Portal
portalPorMapa mapaId = case mapaId of
  PlanicieSerena -> Portal {ondasPortal = [], posicaoPortal = (0.5, 2.5)}
  GargantaPedra -> Portal {ondasPortal = [], posicaoPortal = (2.5, 1.5)}
  LagoFraturado -> Portal {ondasPortal = [], posicaoPortal = (0.5, 5.5)}
  CruzamentoSolar -> Portal {ondasPortal = [], posicaoPortal = (34.5, 2.5)}
  BastiaoEspiral -> Portal {ondasPortal = [], posicaoPortal = (1.5, 2.5)}

criaMapaPlanicie, criaMapaGarganta, criaMapaLago, criaMapaCruzamento, criaMapaBastiao :: [[Terreno]]
criaMapaPlanicie =
  aplicaDecor aguaPlanicie $
    aplicaDecor asfaltoPlanicie $
      aplicaCaminho caminhoPlanicie mapaVazio

criaMapaGarganta =
  aplicaDecor aguaGarganta $
    aplicaDecor asfaltoGarganta $
      aplicaCaminho caminhoGarganta mapaVazio

criaMapaLago =
  aplicaDecor aguaLago $
    aplicaDecor asfaltoLago $
      aplicaCaminho caminhoLago mapaVazio

criaMapaCruzamento =
  aplicaDecor aguaCruzamento $
    aplicaDecor asfaltoCruzamento $
      aplicaCaminho caminhoCruzamento mapaVazio

criaMapaBastiao =
  aplicaDecor aguaBastiao $
    aplicaDecor asfaltoBastiao $
      aplicaCaminho caminhoBastiao mapaVazio

mapaVazio :: [[Terreno]]
mapaVazio = replicate alturaMapaPadrao (replicate larguraMapaPadrao Relva)

aplicaCaminho :: [(Int, Int)] -> [[Terreno]] -> [[Terreno]]
aplicaCaminho coords mapa = foldl (\acc pos -> colocaTerreno Terra pos acc) mapa coords

aplicaDecor :: [(Terreno, (Int, Int))] -> [[Terreno]] -> [[Terreno]]
aplicaDecor decor mapa = foldl (\acc (terreno, pos) -> colocaTerreno terreno pos acc) mapa decor

colocaTerreno :: Terreno -> (Int, Int) -> [[Terreno]] -> [[Terreno]]
colocaTerreno terreno (x, y) mapa =
  [ if linhaIdx == y then alteraLinha linha else linha
    | (linhaIdx, linha) <- zip [0 ..] mapa
  ]
  where
    alteraLinha linha =
      [ if colIdx == x then terreno else celula
        | (colIdx, celula) <- zip [0 ..] linha
      ]

faixaHorizontal :: Int -> Int -> Int -> [(Int, Int)]
faixaHorizontal y x1 x2 = [(x, y) | x <- intervalo x1 x2]

faixaVertical :: Int -> Int -> Int -> [(Int, Int)]
faixaVertical x y1 y2 = [(x, y) | y <- intervalo y1 y2]

retangulo :: Terreno -> Int -> Int -> Int -> Int -> [(Terreno, (Int, Int))]
retangulo terreno x1 y1 x2 y2 =
  [(terreno, (x, y)) | x <- intervalo x1 x2, y <- intervalo y1 y2]

intervalo :: Int -> Int -> [Int]
intervalo a b
  | a <= b = [a .. b]
  | otherwise = reverse [b .. a]

caminhoPlanicie, caminhoGarganta, caminhoLago, caminhoCruzamento, caminhoBastiao :: [(Int, Int)]
caminhoPlanicie =
  faixaHorizontal 2 0 17
    ++ faixaVertical 17 2 22
    ++ faixaHorizontal 22 17 35

caminhoGarganta =
  faixaHorizontal 1 2 10
    ++ faixaVertical 10 1 13
    ++ faixaHorizontal 13 10 28
    ++ faixaVertical 28 13 27
    ++ faixaHorizontal 27 28 31

caminhoLago =
  faixaVertical 0 5 14
    ++ faixaHorizontal 14 0 12
    ++ faixaVertical 12 14 8
    ++ faixaHorizontal 8 12 24
    ++ faixaVertical 24 8 24
    ++ faixaHorizontal 24 24 34

caminhoCruzamento =
  faixaHorizontal 2 34 20
    ++ faixaVertical 20 2 12
    ++ faixaHorizontal 12 20 31
    ++ faixaVertical 31 12 20
    ++ faixaHorizontal 20 31 11
    ++ faixaVertical 11 20 29
    ++ faixaHorizontal 29 11 4

caminhoBastiao =
  faixaHorizontal 2 1 16
    ++ faixaVertical 16 2 9
    ++ faixaHorizontal 9 16 27
    ++ faixaVertical 27 9 21
    ++ faixaHorizontal 21 27 8
    ++ faixaVertical 8 21 28
    ++ faixaHorizontal 28 8 18

aguaPlanicie, aguaGarganta, aguaLago, aguaCruzamento, aguaBastiao :: [(Terreno, (Int, Int))]
aguaPlanicie =
  retangulo Agua 25 4 27 7
    ++ retangulo Agua 3 13 6 18

aguaGarganta =
  retangulo Agua 14 4 18 7
    ++ retangulo Agua 3 18 8 24
    ++ retangulo Agua 23 18 26 21

aguaLago =
  retangulo Agua 4 3 9 7
    ++ retangulo Agua 18 12 23 16
    ++ retangulo Agua 27 4 32 9

aguaCruzamento =
  retangulo Agua 6 4 10 8
    ++ retangulo Agua 24 16 29 20
    ++ retangulo Agua 15 24 19 28

aguaBastiao =
  retangulo Agua 20 4 24 8
    ++ retangulo Agua 3 12 6 16
    ++ retangulo Agua 28 24 33 28

asfaltoPlanicie, asfaltoGarganta, asfaltoLago, asfaltoCruzamento, asfaltoBastiao :: [(Terreno, (Int, Int))]
asfaltoPlanicie = [(Asfalto, (x, 22)) | x <- [21, 24 .. 33]]
asfaltoGarganta = [(Asfalto, (10, y)) | y <- [4, 6 .. 12]] ++ [(Asfalto, (x, 13)) | x <- [15, 18 .. 27]]
asfaltoLago = [(Asfalto, (12, y)) | y <- [10, 12]] ++ [(Asfalto, (x, 8)) | x <- [16, 18 .. 24]]
asfaltoCruzamento = [(Asfalto, (31, y)) | y <- [13, 15 .. 19]] ++ [(Asfalto, (x, 20)) | x <- [16, 18 .. 28]]
asfaltoBastiao = [(Asfalto, (27, y)) | y <- [12, 14 .. 20]] ++ [(Asfalto, (x, 28)) | x <- [10, 12 .. 18]]
