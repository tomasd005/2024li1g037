module UIComponents
  ( UIRect (..),
    ButtonTone (..),
    containsPoint,
    drawButton,
    drawPanel,
    drawLabel,
    drawValue,
  )
where

import Graphics.Gloss

data UIRect = UIRect
  { rectX :: !Float,
    rectY :: !Float,
    rectW :: !Float,
    rectH :: !Float
  }

data ButtonTone = Primary | Neutral | Danger | Disabled
  deriving (Eq)

containsPoint :: (Float, Float) -> UIRect -> Bool
containsPoint (mx, my) (UIRect x y w h) =
  mx >= x - w / 2 && mx <= x + w / 2 && my >= y - h / 2 && my <= y + h / 2

drawPanel :: UIRect -> Picture
drawPanel (UIRect x y w h) =
  Translate x y $
    Pictures
      [ Color (makeColorI 22 29 25 238) $ rectangleSolid w h,
        Color (makeColorI 69 85 69 255) $ rectangleWire w h
      ]

drawButton :: Maybe (Float, Float) -> UIRect -> ButtonTone -> String -> Picture
drawButton mouse rect@(UIRect x y w h) tone label =
  let hovered = maybe False (`containsPoint` rect) mouse && tone /= Disabled
      (fillBase, borderBase, textColor) = colors tone
      fill = if hovered then brighten fillBase else fillBase
      border = if hovered then makeColorI 235 204 105 255 else borderBase
      scaleText = min 0.105 (max 0.062 (w / max 1 (fromIntegral (length label)) / 86))
      textWidth = fromIntegral (length label) * 104 * scaleText
   in Translate x y $
        Pictures
          [ Color fill $ rectangleSolid w h,
            Color border $ rectangleWire w h,
            if hovered
              then Color (withAlpha 0.18 border) $ rectangleSolid (w - 8) (h - 8)
              else Blank,
            Translate (-textWidth / 2) (-9) $ Scale scaleText scaleText $ Color textColor $ Text label
          ]

drawLabel :: Float -> Float -> String -> Picture
drawLabel x y label =
  Translate x y $ Scale 0.062 0.062 $ Color (makeColorI 157 171 151 255) $ Text label

drawValue :: Float -> Float -> String -> Color -> Picture
drawValue x y value colorValue =
  Translate x y $ Scale 0.105 0.105 $ Color colorValue $ Text value

colors :: ButtonTone -> (Color, Color, Color)
colors Primary = (makeColorI 67 80 48 242, makeColorI 226 194 95 255, makeColorI 238 241 229 255)
colors Neutral = (makeColorI 32 42 35 236, makeColorI 91 108 87 255, makeColorI 226 232 218 255)
colors Danger = (makeColorI 76 42 38 242, makeColorI 190 82 72 255, makeColorI 246 228 220 255)
colors Disabled = (makeColorI 36 38 36 210, makeColorI 69 73 68 255, makeColorI 120 126 116 255)

brighten :: Color -> Color
brighten baseColor =
  let (r, g, b, a) = rgbaOfColor baseColor
   in makeColor (min 1 (r + 0.08)) (min 1 (g + 0.08)) (min 1 (b + 0.08)) a
