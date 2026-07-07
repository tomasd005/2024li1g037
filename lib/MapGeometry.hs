module MapGeometry
  ( MapLayoutConfig (..),
    MapDimensions (..),
    dimensoesMapa,
    dentroMapa,
    terrenoEmCelula,
    atualizaCelula,
    tamanhoBloco,
    offsetMapa,
    mapaParaEcra,
    ecraParaCelula,
  )
where

import LI12425

data MapLayoutConfig = MapLayoutConfig
  { layoutLarguraMax :: Float,
    layoutAlturaMax :: Float,
    layoutCentroY :: Float
  }

data MapDimensions = MapDimensions
  { larguraMapa :: Int,
    alturaMapa :: Int
  }

dimensoesMapa :: Mapa -> Maybe MapDimensions
dimensoesMapa [] = Nothing
dimensoesMapa ([] : _) = Nothing
dimensoesMapa mapa@(linha : _) = Just (MapDimensions (length linha) (length mapa))

dentroMapa :: Mapa -> Int -> Int -> Bool
dentroMapa mapa x y =
  case dimensoesMapa mapa of
    Nothing -> False
    Just dims -> x >= 0 && y >= 0 && x < larguraMapa dims && y < alturaMapa dims

terrenoEmCelula :: Mapa -> Int -> Int -> Maybe Terreno
terrenoEmCelula mapa x y = do
  linha <- indiceSeguro y mapa
  indiceSeguro x linha

indiceSeguro :: Int -> [a] -> Maybe a
indiceSeguro n _
  | n < 0 = Nothing
indiceSeguro _ [] = Nothing
indiceSeguro 0 (x : _) = Just x
indiceSeguro n (_ : xs) = indiceSeguro (n - 1) xs

atualizaCelula :: Int -> Int -> Terreno -> Mapa -> Mapa
atualizaCelula cx cy novoTerreno mapa =
  [ [ if x == cx && y == cy then novoTerreno else terreno
      | (x, terreno) <- zip [0 :: Int ..] linha
    ]
    | (y, linha) <- zip [0 :: Int ..] mapa
  ]

tamanhoBloco :: MapLayoutConfig -> Mapa -> Maybe Float
tamanhoBloco cfg mapa = do
  dims <- dimensoesMapa mapa
  let largura = fromIntegral (larguraMapa dims)
      altura = fromIntegral (alturaMapa dims)
  return (min (layoutLarguraMax cfg / largura) (layoutAlturaMax cfg / altura))

offsetMapa :: MapLayoutConfig -> Mapa -> Maybe (Float, Float)
offsetMapa cfg mapa = do
  dims <- dimensoesMapa mapa
  bloco <- tamanhoBloco cfg mapa
  let largura = fromIntegral (larguraMapa dims)
      altura = fromIntegral (alturaMapa dims)
  return (-(largura * bloco) / 2, layoutCentroY cfg - (altura * bloco) / 2)

mapaParaEcra :: MapLayoutConfig -> Mapa -> Posicao -> Maybe (Float, Float)
mapaParaEcra cfg mapa (x, y) = do
  dims <- dimensoesMapa mapa
  bloco <- tamanhoBloco cfg mapa
  (offsetX, offsetY) <- offsetMapa cfg mapa
  let altura = fromIntegral (alturaMapa dims)
  return (offsetX + x * bloco, offsetY + (altura - y) * bloco)

ecraParaCelula :: MapLayoutConfig -> Mapa -> (Float, Float) -> Maybe (Int, Int)
ecraParaCelula cfg mapa (mx, my) = do
  dims <- dimensoesMapa mapa
  bloco <- tamanhoBloco cfg mapa
  (offsetX, offsetY) <- offsetMapa cfg mapa
  let altura = fromIntegral (alturaMapa dims)
      cx = floor ((mx - offsetX) / bloco)
      cy = floor (altura - ((my - offsetY) / bloco))
  if cx >= 0 && cy >= 0 && cx < larguraMapa dims && cy < alturaMapa dims
    then Just (cx, cy)
    else Nothing
