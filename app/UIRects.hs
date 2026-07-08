module UIRects
  ( startWaveRect,
    pauseRect,
    speed1Rect,
    speed2Rect,
    speed4Rect,
    upgradeRect,
    sellRect,
    cancelRect,
    hudToggleRect,
    shopToggleRect,
    resumeRect,
    restartRect,
    menuRect,
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

import ImmutableTowers (alturaJanela)
import UIComponents

startWaveRect, pauseRect, speed1Rect, speed2Rect, speed4Rect, upgradeRect, sellRect, cancelRect :: UIRect
startWaveRect = UIRect 464 494 138 42
pauseRect = UIRect 588 494 46 42
speed1Rect = UIRect 648 494 52 42
speed2Rect = UIRect 706 494 52 42
speed4Rect = UIRect 764 494 52 42
upgradeRect = UIRect 772 (-66) 122 42
sellRect = UIRect 772 (-116) 122 42
cancelRect = UIRect 772 (-166) 122 42

hudToggleRect, shopToggleRect :: UIRect
hudToggleRect = UIRect 844 494 72 42
shopToggleRect = UIRect 924 494 84 42

resumeRect, restartRect, menuRect :: UIRect
resumeRect = UIRect 0 (-28) 190 52
restartRect = UIRect 0 (-92) 190 52
menuRect = UIRect 0 (-156) 190 52

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
      largura = min 620 (172 + fromIntegral slots * 96)
   in UIRect (-236) (-alturaJanela / 2 + 74) largura 110

shopSlotCenter :: Int -> Int -> (Float, Float)
shopSlotCenter total indice =
  let UIRect panelX panelY panelW _ = shopPanelRect total
      startX = panelX - panelW / 2 + 78
      step = 96
   in (startX + fromIntegral indice * step, panelY)

modeHistoriaRect, modeInfinitoRect, modeDesafioRect, modeBossRect, modeSandboxRect :: UIRect
modeHistoriaRect = UIRect (-360) 90 300 152
modeInfinitoRect = UIRect 0 90 300 152
modeDesafioRect = UIRect 360 90 300 152
modeBossRect = UIRect (-180) (-105) 300 152
modeSandboxRect = UIRect 180 (-105) 300 152
