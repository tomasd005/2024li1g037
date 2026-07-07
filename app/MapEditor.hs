module MapEditor
  ( cicloCelulaMapa,
    colocaObstaculo,
  )
where

import LI12425
import MapGeometry

cicloCelulaMapa :: MapLayoutConfig -> (Float, Float) -> Mapa -> Mapa
cicloCelulaMapa cfg pos mapa =
  case ecraParaCelula cfg mapa pos of
    Just (cx, cy) ->
      case terrenoEmCelula mapa cx cy of
        Just atual -> atualizaCelula cx cy (proximoTerreno atual) mapa
        Nothing -> mapa
    Nothing -> mapa

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
