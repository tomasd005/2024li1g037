module RenderConfig
  ( gameLayout,
    layoutParaJanela,
    gameMapCenterY,
  )
where

import MapGeometry

mapaLarguraMax, mapaAlturaMax, mapaCentroY :: Float
mapaLarguraMax = 1280
mapaAlturaMax = 952
mapaCentroY = 18

gameLayout :: MapLayoutConfig
gameLayout = MapLayoutConfig mapaLarguraMax mapaAlturaMax mapaCentroY

layoutParaJanela :: (Int, Int) -> MapLayoutConfig
layoutParaJanela (w, h) =
  let largura = fromIntegral (max 1280 w)
      altura = fromIntegral (max 720 h)
      painelDireito = min 360 (largura * 0.18)
      barraInferior = min 148 (altura * 0.14)
      barraSuperior = min 108 (altura * 0.1)
      larguraMapa = max 920 (largura - painelDireito - 120)
      alturaMapa = max 620 (altura - barraInferior - barraSuperior - 64)
      centroY = (barraInferior - barraSuperior) * 0.22
   in MapLayoutConfig larguraMapa alturaMapa centroY

gameMapCenterY :: Float
gameMapCenterY = mapaCentroY
