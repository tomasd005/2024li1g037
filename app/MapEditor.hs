module MapEditor
  ( cicloCelulaMapa,
    cicloCelulaMapaValidado,
    colocaObstaculo,
  )
where

import LI12425
import MapGeometry
import Tarefa1 (buscaCaminho, terrenoCaminhavel)

cicloCelulaMapa :: MapLayoutConfig -> (Float, Float) -> Mapa -> Mapa
cicloCelulaMapa cfg pos mapa =
  case ecraParaCelula cfg mapa pos of
    Just (cx, cy) ->
      case terrenoEmCelula mapa cx cy of
        Just atual -> atualizaCelula cx cy (proximoTerreno atual) mapa
        Nothing -> mapa
    Nothing -> mapa

cicloCelulaMapaValidado :: MapLayoutConfig -> (Float, Float) -> Jogo -> Either String Jogo
cicloCelulaMapaValidado cfg pos jogoAtual =
  let mapaNovo = cicloCelulaMapa cfg pos (mapaJogo jogoAtual)
      base = baseJogo jogoAtual
      portais = portaisJogo jogoAtual
      caminhoValido portal = buscaCaminho mapaNovo (posicaoPortal portal) (posicaoBase base) []
      torresValidas = all (\torre -> terrenoEmPosicao mapaNovo (posicaoTorre torre) == Just Relva) (torresJogo jogoAtual)
   in if not (terrenoCaminhavel (posicaoBase base) mapaNovo)
        then Left "A base tem de permanecer no caminho"
        else if any (not . (`terrenoCaminhavel` mapaNovo) . posicaoPortal) portais
          then Left "Os portais tem de permanecer no caminho"
          else if not torresValidas
            then Left "Nao podes alterar o terreno de uma torre"
            else if not (all caminhoValido portais)
              then Left "A edicao bloqueava o caminho ate a base"
              else Right jogoAtual {mapaJogo = mapaNovo}

terrenoEmPosicao :: Mapa -> Posicao -> Maybe Terreno
terrenoEmPosicao mapa (x, y) = terrenoEmCelula mapa (floor x) (floor y)

colocaObstaculo :: MapLayoutConfig -> (Float, Float) -> Mapa -> Mapa
colocaObstaculo cfg pos mapa =
  case ecraParaCelula cfg mapa pos of
    Just (cx, cy) -> case terrenoEmCelula mapa cx cy of
      Just Terra -> atualizaCelula cx cy Relva mapa
      Just Asfalto -> atualizaCelula cx cy Relva mapa
      _ -> mapa
    Nothing -> mapa

proximoTerreno :: Terreno -> Terreno
proximoTerreno terreno = case terreno of
  Relva -> Terra
  Terra -> Asfalto
  Asfalto -> Agua
  Agua -> Relva
