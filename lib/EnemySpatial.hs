module EnemySpatial
  ( SpatialGrid,
    buildSpatialGrid,
    queryEnemyIndices,
  )
where

import qualified Data.IntMap.Strict as IntMap
import LI12425

data SpatialGrid = SpatialGrid !Float !(IntMap.IntMap [Int])

buildSpatialGrid :: Float -> [(Int, Inimigo)] -> SpatialGrid
buildSpatialGrid cellSize inimigos =
  SpatialGrid tamanhoCelula $
    foldr inserir IntMap.empty inimigos
  where
    tamanhoCelula = max 1 cellSize
    inserir (indice, inimigo) =
      let chave = chaveCelula tamanhoCelula (posicaoInimigo inimigo)
       in IntMap.insertWith (++) chave [indice]

queryEnemyIndices :: SpatialGrid -> Posicao -> Float -> [Int]
queryEnemyIndices (SpatialGrid cellSize cells) (x, y) raio =
  concatMap inimigosNaCelula chaves
  where
    minX = floor ((x - raio) / cellSize)
    maxX = floor ((x + raio) / cellSize)
    minY = floor ((y - raio) / cellSize)
    maxY = floor ((y + raio) / cellSize)
    chaves = [chaveCoordenadas cx cy | cx <- [minX .. maxX], cy <- [minY .. maxY]]
    inimigosNaCelula chave = IntMap.findWithDefault [] chave cells

chaveCelula :: Float -> Posicao -> Int
chaveCelula cellSize (x, y) =
  chaveCoordenadas (floor (x / cellSize)) (floor (y / cellSize))

chaveCoordenadas :: Int -> Int -> Int
chaveCoordenadas x y =
  let zx = zigZag x
      zy = zigZag y
      soma = zx + zy
   in (soma * (soma + 1)) `div` 2 + zy

zigZag :: Int -> Int
zigZag n
  | n >= 0 = n * 2
  | otherwise = (-n * 2) - 1
