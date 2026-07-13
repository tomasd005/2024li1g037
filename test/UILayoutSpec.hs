module UILayoutSpec (testesUILayout) where

import ImmutableTowers (alturaJanela, larguraJanela)
import RenderConfig (layoutParaJanela)
import Test.HUnit
import MapGeometry (MapLayoutConfig (..))
import UIComponents (UIRect (..))
import UIRects

testesUILayout :: Test
testesUILayout =
  TestLabel "UI layout" $
    test
      [ "top controls stay inside top bar" ~: assertBool "Top controls must fit inside the HUD bar" allTopControlsInside,
        "game panel stays inside screen" ~: assertBool "Game panel must fit inside the screen" gamePanelInsideScreen,
        "panel actions stay inside game panel" ~: assertBool "Panel actions must fit inside the panel" panelActionsInside,
        "shop panel stays above bottom edge" ~: assertBool "Shop panel must stay visible inside the screen" shopPanelInsideScreen,
        "shop slots stay inside shop panel" ~: assertBool "Shop slots must fit inside the panel" shopSlotsInsidePanel,
        "responsive layouts keep enough usable map area" ~: assertBool "Representative resolutions must preserve useful map space" layoutResponsiveEnough
      ]

allTopControlsInside :: Bool
allTopControlsInside = all (`rectInside` topBarRect) topControls

gamePanelInsideScreen :: Bool
gamePanelInsideScreen = gamePanelRect `rectInside` screenRect

panelActionsInside :: Bool
panelActionsInside = all (`rectInside` gamePanelRect) panelActions

shopPanelInsideScreen :: Bool
shopPanelInsideScreen =
  let UIRect _ y _ h = shopPanelRect 2
   in y - h / 2 > (-alturaJanela / 2)

shopSlotsInsidePanel :: Bool
shopSlotsInsidePanel =
  let panel = shopPanelRect 2
      slotRects =
        [ let (x, y) = shopSlotCenter 2 i
           in UIRect x y 82 96
        | i <- [0, 1]
        ]
   in all (`rectInside` panel) slotRects

layoutResponsiveEnough :: Bool
layoutResponsiveEnough =
  all valido resolucoesTeste
  where
    resolucoesTeste =
      [ (1280, 720)
      , (1600, 900)
      , (1920, 1080)
      , (2560, 1440)
      ]
    valido resolucao =
      let cfg = layoutParaJanela resolucao
       in larguraUtil cfg >= 920 && alturaUtil cfg >= 620

larguraUtil :: MapLayoutConfig -> Float
larguraUtil = layoutLarguraMax

alturaUtil :: MapLayoutConfig -> Float
alturaUtil = layoutAlturaMax

topBarRect :: UIRect
topBarRect = UIRect 0 (alturaJanela / 2 - 42) (larguraJanela - 48) 82

screenRect :: UIRect
screenRect = UIRect 0 0 larguraJanela alturaJanela

topControls :: [UIRect]
topControls =
  [ pauseRect,
    speed1Rect,
    speed2Rect,
    speed4Rect,
    autoBotRect,
    hudToggleRect,
    shopToggleRect
  ]

panelActions :: [UIRect]
panelActions =
  [ upgradeRect,
    specializationARect,
    specializationBRect,
    sellRect,
    cancelRect
  ]

rectInside :: UIRect -> UIRect -> Bool
rectInside (UIRect x y w h) (UIRect ox oy ow oh) =
  x - w / 2 >= ox - ow / 2
    && x + w / 2 <= ox + ow / 2
    && y - h / 2 >= oy - oh / 2
    && y + h / 2 <= oy + oh / 2
