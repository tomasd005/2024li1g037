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
    modeHistoriaRect,
    modeInfinitoRect,
    modeDesafioRect,
    modeBossRect,
    modeSandboxRect,
  )
where

import UIComponents

startWaveRect, pauseRect, speed1Rect, speed2Rect, speed4Rect, upgradeRect, sellRect, cancelRect :: UIRect
startWaveRect = UIRect 210 (-424) 132 42
pauseRect = UIRect 310 (-424) 64 42
speed1Rect = UIRect 384 (-424) 48 42
speed2Rect = UIRect 438 (-424) 48 42
speed4Rect = UIRect 492 (-424) 48 42
upgradeRect = UIRect 430 (-98) 72 38
sellRect = UIRect 430 (-144) 72 38
cancelRect = UIRect 430 (-190) 72 38

hudToggleRect, shopToggleRect :: UIRect
hudToggleRect = UIRect 548 (-424) 48 42
shopToggleRect = UIRect 602 (-424) 48 42

resumeRect, restartRect, menuRect :: UIRect
resumeRect = UIRect 0 (-28) 190 52
restartRect = UIRect 0 (-92) 190 52
menuRect = UIRect 0 (-156) 190 52

menuHeroRect, menuModeRect, menuShopRect, menuProfileRect, menuLeaderboardRect, menuHelpRect, menuOptionsRect, menuExitRect :: UIRect
menuHeroRect = UIRect (-180) 38 360 118
menuModeRect = UIRect 172 38 238 118
menuShopRect = UIRect (-420) (-150) 132 52
menuProfileRect = UIRect (-270) (-150) 132 52
menuLeaderboardRect = UIRect (-120) (-150) 132 52
menuHelpRect = UIRect 30 (-150) 132 52
menuOptionsRect = UIRect 180 (-150) 132 52
menuExitRect = UIRect 330 (-150) 112 52

modeHistoriaRect, modeInfinitoRect, modeDesafioRect, modeBossRect, modeSandboxRect :: UIRect
modeHistoriaRect = UIRect (-360) 90 300 152
modeInfinitoRect = UIRect 0 90 300 152
modeDesafioRect = UIRect 360 90 300 152
modeBossRect = UIRect (-180) (-105) 300 152
modeSandboxRect = UIRect 180 (-105) 300 152
