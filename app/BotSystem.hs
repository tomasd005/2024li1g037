module BotSystem
  ( botColocaTorre,
    botExecutaAcao,
    sugestaoBot,
  )
where

import Data.Function (on)
import Data.List (minimumBy)
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

botExecutaAcao :: Jogo -> Maybe (Jogo, String)
botExecutaAcao jogoAtual
  | deveIniciarOnda jogoAtual = Just (iniciaProximaVagaBot jogoAtual, "Bot iniciou vaga")
  | Just jogoComUpgrade <- tentaUpgradeBot jogoAtual = Just (jogoComUpgrade, "Bot melhorou uma torre")
  | Just jogoComTorre <- tentaConstruirBot jogoAtual = Just (jogoComTorre, "Bot construiu uma torre")
  | otherwise = Nothing

sugestaoBot :: Jogo -> String
sugestaoBot jogoAtual
  | null (torresJogo jogoAtual) = "compra Resina"
  | creditosBase base >= custoUpgradeMaisBarato = "usa U"
  | vidaBase base < 30 = "Gelo/Medo"
  | length (inimigosJogo jogoAtual) > 4 = "Eletrico"
  | otherwise = "guardar"
  where
    base = baseJogo jogoAtual
    custoUpgradeMaisBarato = foldl' min 9999 (map custoUpgradeTorre (torresJogo jogoAtual))

deveIniciarOnda :: Jogo -> Bool
deveIniciarOnda jogoAtual =
  null (inimigosJogo jogoAtual)
    && any temOndaPorComecar (portaisJogo jogoAtual)
  where
    temOndaPorComecar portal =
      case ondasPortal portal of
        [] -> False
        onda : _ -> not (null (inimigosOnda onda)) && entradaOnda onda > 0

iniciaProximaVagaBot :: Jogo -> Jogo
iniciaProximaVagaBot jogoAtual =
  jogoAtual {portaisJogo = map ativaPortal (portaisJogo jogoAtual)}
  where
    ativaPortal portal =
      case ondasPortal portal of
        [] -> portal
        onda : resto -> portal {ondasPortal = onda {entradaOnda = 0, tempoOnda = 0} : resto}

tentaUpgradeBot :: Jogo -> Maybe Jogo
tentaUpgradeBot jogoAtual =
  case torresElegiveis of
    [] -> Nothing
    torre : _ ->
      let custo = custoUpgradeTorre torre
          baseAtual = baseJogo jogoAtual
          baseNova = baseAtual {creditosBase = creditosBase baseAtual - custo}
       in Just jogoAtual {torresJogo = substituirTorre torre (upgradeTorre torre) (torresJogo jogoAtual), baseJogo = baseNova}
  where
    creditoAtual = creditosBase (baseJogo jogoAtual)
    torresElegiveis =
      [ torre
      | torre <- torresJogo jogoAtual,
        custoUpgradeTorre torre <= creditoAtual
      ]

tentaConstruirBot :: Jogo -> Maybe Jogo
tentaConstruirBot jogoAtual =
  case filter (\(preco, _) -> creditosBase (baseJogo jogoAtual) >= preco) (lojaJogo jogoAtual) of
    [] -> Nothing
    opcoes ->
      case melhorCelulaParaBot jogoAtual of
        Nothing -> Nothing
        Just pos ->
          let (preco, torre) = minimumBy (compare `on` fst) opcoes
              novaBase = (baseJogo jogoAtual) {creditosBase = creditosBase (baseJogo jogoAtual) - preco}
           in Just jogoAtual {baseJogo = novaBase, torresJogo = torre {posicaoTorre = pos} : torresJogo jogoAtual}

substituirTorre :: Torre -> Torre -> [Torre] -> [Torre]
substituirTorre alvo nova =
  map (\torre -> if posicaoTorre torre == posicaoTorre alvo then nova else torre)

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
