module UILayoutSpec (testesUILayout) where

import ImmutableTowers (alturaJanela, larguraJanela)
import Test.HUnit
import UIComponents (UIRect (..))
import UIRects

testesUILayout :: Test
testesUILayout =
  TestLabel "UI layout" $
    test
      [ "top controls stay inside top bar" ~: assertBool "Top controls must fit inside the HUD bar" allTopControlsInside,
        "shop panel stays above bottom edge" ~: assertBool "Shop panel must stay visible inside the screen" shopPanelInsideScreen,
        "shop slots stay inside shop panel" ~: assertBool "Shop slots must fit inside the panel" shopSlotsInsidePanel
      ]

allTopControlsInside :: Bool
allTopControlsInside = all (`rectInside` topBarRect) topControls

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

topBarRect :: UIRect
topBarRect = UIRect 0 (alturaJanela / 2 - 42) (larguraJanela - 48) 82

topControls :: [UIRect]
topControls =
  [ startWaveRect,
    pauseRect,
    speed1Rect,
    speed2Rect,
    speed4Rect,
    hudToggleRect,
    shopToggleRect
  ]

rectInside :: UIRect -> UIRect -> Bool
rectInside (UIRect x y w h) (UIRect ox oy ow oh) =
  x - w / 2 >= ox - ow / 2
    && x + w / 2 <= ox + ow / 2
    && y - h / 2 >= oy - oh / 2
    && y + h / 2 <= oy + oh / 2
