module MenuComponents
  ( drawHeroCard,
    drawUtilityCard,
    drawSummaryPanel,
    drawModalPanel,
    drawSidebarButton,
  )
where

import Graphics.Gloss
import UIComponents
import UIText

drawHeroCard :: UIRect -> Bool -> String -> String -> Picture
drawHeroCard (UIRect x y w h) selecionado titulo subtitulo =
  let corBorda = if selecionado then makeColorI 226 194 95 255 else makeColorI 92 103 88 255
      corFundo = if selecionado then makeColorI 55 60 45 235 else makeColorI 31 39 33 225
   in Pictures
        [ Color corFundo $ Translate x y $ rectangleSolid w h,
          Color corBorda $ Translate x y $ rectangleWire w h,
          if selecionado then drawAccentBar x (y + h / 2 - 8) w corBorda else Blank,
          drawUITextLeft (x - w / 2 + 26) (y + 26) 5 corTexto titulo,
          drawUITextLeft (x - w / 2 + 26) (y - 8) 2.4 corTextoSub subtitulo
        ]

drawUtilityCard :: Maybe (Float, Float) -> UIRect -> Bool -> String -> Picture
drawUtilityCard mouse rect@(UIRect x y w h) selecionado titulo =
  let corBorda = if selecionado then makeColorI 226 194 95 255 else makeColorI 92 103 88 255
   in Pictures
        [ drawButton mouse rect (if selecionado then Primary else Neutral) titulo,
          if selecionado then drawAccentBar x (y + h / 2 - 8) w corBorda else Blank
        ]

drawSidebarButton :: Maybe (Float, Float) -> UIRect -> Bool -> String -> String -> Picture
drawSidebarButton mouse rect@(UIRect x y w h) selecionado titulo subtitulo =
  let hovered = maybe False (`containsPoint` rect) mouse
      corBorda
        | selecionado = makeColorI 226 194 95 255
        | hovered = makeColorI 188 198 178 255
        | otherwise = makeColorI 83 97 84 255
      corFundo
        | selecionado = makeColorI 53 61 47 236
        | hovered = makeColorI 37 46 39 232
        | otherwise = makeColorI 28 36 31 224
   in Pictures
        [ Color corFundo $ Translate x y $ rectangleSolid w h,
          Color corBorda $ Translate x y $ rectangleWire w h,
          if selecionado then drawAccentBar x (y + h / 2 - 8) w corBorda else Blank,
          drawGlossLabel (x - w / 2 + 24) (y + 8) 0.17 corTexto titulo,
          drawGlossLabel (x - w / 2 + 24) (y - 15) 0.067 corTextoSub subtitulo
        ]

drawSummaryPanel :: Float -> Float -> [String] -> Picture
drawSummaryPanel x y linhas =
  Pictures
    [ Color (withAlpha 0.92 corPainel) $ Translate x y $ rectangleSolid 860 126,
      Color (makeColorI 68 78 63 255) $ Translate x y $ rectangleWire 860 126,
      drawUITextLeft (x - 392) (y + 30) 3.2 corTitulo "RESUMO",
      Pictures
        [ drawUITextLeft (x - 392 + fromIntegral (i `mod` 2) * 396) (y - 4 - fromIntegral (i `div` 2) * 32) 2.2 corTexto ("+ " ++ linha)
          | (i, linha) <- zip [0 :: Int ..] linhas
        ]
    ]

drawModalPanel :: Float -> Float -> String -> [String] -> Picture
drawModalPanel w h titulo linhas =
  Pictures
    [ Color corPainel $ rectangleSolid w h,
      Color (makeColorI 68 78 63 255) $ rectangleWire w h,
      drawGlossLabel (-w / 2 + 92) (h / 2 - 84) 0.28 corTitulo titulo,
      Pictures
        [ drawGlossLabel (-w / 2 + 92) (h / 2 - 156 - fromIntegral i * 40) 0.11 corTexto linha
          | (i, linha) <- zip [0 :: Int ..] linhas
        ]
    ]

corPainel, corTexto, corTextoSub, corTitulo :: Color
corPainel = makeColorI 21 27 24 232
corTexto = makeColorI 229 233 223 255
corTextoSub = makeColorI 154 164 146 255
corTitulo = makeColorI 226 194 95 255

drawGlossLabel :: Float -> Float -> Float -> Color -> String -> Picture
drawGlossLabel x y escala corValor texto =
  Pictures
    [ Translate (x + 2) (y - 2) $ Scale escala escala $ Color (withAlpha 0.22 black) $ Text texto,
      Translate x y $ Scale escala escala $ Color corValor $ Text texto
    ]
