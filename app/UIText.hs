module UIText
  ( drawUITextLeft,
    drawUITextCentered,
    uiTextWidth,
  )
where

import Data.Char (toUpper)
import Graphics.Gloss

drawUITextLeft :: Float -> Float -> Float -> Color -> String -> Picture
drawUITextLeft x y pixel colorValue textValue =
  Pictures
    [ Translate (x + pixel * 0.24) (y - pixel * 0.24) $
        Scale escala escala $
        Color (withAlpha 0.22 black) $
        Text texto,
      Translate x y $
        Scale escala escala $
        Color colorValue $
        Text texto
    ]
  where
    texto = map toUpper textValue
    escala = pixelToScale pixel

drawUITextCentered :: Float -> Float -> Float -> Color -> String -> Picture
drawUITextCentered x y pixel colorValue textValue =
  drawUITextLeft (x - uiTextWidth pixel textValue / 2) y pixel colorValue textValue

uiTextWidth :: Float -> String -> Float
uiTextWidth pixel textValue =
  max 0 (fromIntegral (length textValue) * pixel * 5.8 - pixel * 0.8)

pixelToScale :: Float -> Float
pixelToScale pixel = max 0.06 (pixel * 0.05)
