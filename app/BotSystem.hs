module BotSystem
  ( botColocaTorre,
    sugestaoBot,
  )
where

import LI12425
import MapGeometry
import TowerSystem

botColocaTorre :: Jogo -> Jogo
botColocaTorre jogoAtual =
  case filter (\(preco, _) -> creditosBase (baseJogo jogoAtual) >= preco) (lojaJogo jogoAtual) of
    [] -> jogoAtual
    (preco, torre) : _ -> case melhorCelulaParaBot jogoAtual of
      Nothing -> jogoAtual
      Just pos ->
        let novaBase = (baseJogo jogoAtual) {creditosBase = creditosBase (baseJogo jogoAtual) - preco}
         in jogoAtual {baseJogo = novaBase, torresJogo = torre {posicaoTorre = pos} : torresJogo jogoAtual}

sugestaoBot :: Jogo -> String
sugestaoBot jogoAtual
  | null (torresJogo jogoAtual) = "compra Resina"
  | creditosBase base >= custoUpgradeMaisBarato = "usa U"
  | vidaBase base < 30 = "Gelo/Medo"
  | length (inimigosJogo jogoAtual) > 4 = "Eletrico"
  | otherwise = "guardar"
  where
    base = baseJogo jogoAtual
    custoUpgradeMaisBarato = minimum (map custoUpgradeTorre (torresJogo jogoAtual) ++ [9999])

melhorCelulaParaBot :: Jogo -> Maybe Posicao
melhorCelulaParaBot jogoAtual =
  let mapa = mapaJogo jogoAtual
      ocupadas = map posicaoTorre (torresJogo jogoAtual)
      candidatas =
        [ (fromIntegral x + 0.5, fromIntegral y + 0.5)
          | (y, linha) <- zip [0 :: Int ..] mapa,
            (x, terreno) <- zip [0 :: Int ..] linha,
            terreno == Relva,
            (fromIntegral x + 0.5, fromIntegral y + 0.5) `notElem` ocupadas,
            pertoDoCaminho x y mapa
        ]
   in case candidatas of
      [] -> Nothing
      (pos:_) -> Just pos

pertoDoCaminho :: Int -> Int -> Mapa -> Bool
pertoDoCaminho x y mapa =
  any caminhavel [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    caminhavel (cx, cy)
      | otherwise = terrenoEmCelula mapa cx cy `elem` [Just Terra, Just Asfalto]
