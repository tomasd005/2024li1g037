module UIRects
  ( pauseRect,
    speed1Rect,
    speed2Rect,
    speed4Rect,
    autoBotRect,
    upgradeRect,
    sellRect,
    cancelRect,
    hudToggleRect,
    shopToggleRect,
    resumeRect,
    restartRect,
    menuRect,
    resultMenuRect,
    resultReplayRect,
    menuHeroRect,
    menuModeRect,
    menuShopRect,
    menuProfileRect,
    menuLeaderboardRect,
    menuHelpRect,
    menuOptionsRect,
    menuExitRect,
    shopPanelRect,
    shopSlotCenter,
    modeHistoriaRect,
    modeInfinitoRect,
    modeDesafioRect,
    modeBossRect,
    modeSandboxRect,
  )
where

import ImmutableTowers (larguraJanela)
import UIComponents

pauseRect, speed1Rect, speed2Rect, speed4Rect, autoBotRect, upgradeRect, sellRect, cancelRect :: UIRect
pauseRect = UIRect 418 494 54 42
speed1Rect = UIRect 476 494 52 42
speed2Rect = UIRect 534 494 52 42
speed4Rect = UIRect 592 494 56 42
autoBotRect = UIRect 660 494 74 42
upgradeRect = UIRect 772 (-66) 122 42
sellRect = UIRect 772 (-116) 122 42
cancelRect = UIRect 772 (-166) 122 42

hudToggleRect, shopToggleRect :: UIRect
hudToggleRect = UIRect 732 494 64 42
shopToggleRect = UIRect 812 494 80 42

resumeRect, restartRect, menuRect, resultMenuRect, resultReplayRect :: UIRect
resumeRect = UIRect 0 (-28) 190 52
restartRect = UIRect 0 (-92) 190 52
menuRect = UIRect 0 (-156) 190 52
resultMenuRect = UIRect (-108) (-156) 190 52
resultReplayRect = UIRect 108 (-156) 190 52

menuHeroRect, menuModeRect, menuShopRect, menuProfileRect, menuLeaderboardRect, menuHelpRect, menuOptionsRect, menuExitRect :: UIRect
menuHeroRect = UIRect (-520) 132 280 68
menuModeRect = UIRect (-520) 54 280 68
menuShopRect = UIRect (-520) (-22) 280 56
menuProfileRect = UIRect (-520) (-90) 280 56
menuLeaderboardRect = UIRect (-520) (-158) 280 56
menuHelpRect = UIRect (-520) (-226) 280 56
menuOptionsRect = UIRect (-520) (-294) 280 56
menuExitRect = UIRect (-520) (-362) 280 56

shopPanelRect :: Int -> UIRect
shopPanelRect total =
  let slots = max 2 total
      colunas :: Int
      colunas = 2
      linhas :: Int
      linhas = ceiling (fromIntegral slots / fromIntegral colunas :: Float)
      largura = 224
      altura = max 250 (124 + fromIntegral linhas * 104)
      posX = -larguraJanela / 2 + largura / 2 + 34
   in UIRect posX (-28) largura altura

shopSlotCenter :: Int -> Int -> (Float, Float)
shopSlotCenter total indice =
  let UIRect panelX panelY panelW panelH = shopPanelRect total
      coluna = indice `mod` 2
      linha = indice `div` 2
      startX = panelX - panelW / 2 + 64
      startY = panelY + panelH / 2 - 122
      stepX = 96
      stepY = 104
   in (startX + fromIntegral coluna * stepX, startY - fromIntegral linha * stepY)

modeHistoriaRect, modeInfinitoRect, modeDesafioRect, modeBossRect, modeSandboxRect :: UIRect
modeHistoriaRect = UIRect (-360) 90 300 152
modeInfinitoRect = UIRect 0 90 300 152
modeDesafioRect = UIRect 360 90 300 152
modeBossRect = UIRect (-180) (-105) 300 152
modeSandboxRect = UIRect 180 (-105) 300 152
