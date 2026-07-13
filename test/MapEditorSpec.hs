module MapEditorSpec (testesMapEditor) where

import LI12425
import MapEditor
import MapGeometry
import Test.HUnit

testesMapEditor :: Test
testesMapEditor =
  TestLabel "Map editor" $
    test
      [ "rejects an edit that blocks every path" ~: bloqueioDoCaminho,
        "accepts an edit away from the active path" ~: edicaoSegura
      ]

bloqueioDoCaminho :: Assertion
bloqueioDoCaminho =
  case posicaoEcra (1.5, 1.5) of
    Nothing -> assertFailure "Expected a screen position for the central cell"
    Just pos -> case cicloCelulaMapaValidado layoutTeste pos jogoTeste of
      Left _ -> pure ()
      Right _ -> assertFailure "The editor accepted a map with no portal-to-base path"

edicaoSegura :: Assertion
edicaoSegura =
  case posicaoEcra (1.5, 0.5) of
    Nothing -> assertFailure "Expected a screen position for a grass cell"
    Just pos -> case cicloCelulaMapaValidado layoutTeste pos jogoTeste of
      Left erro -> assertFailure ("A safe edit was rejected: " ++ erro)
      Right jogoEditado -> assertEqual "The selected grass cell should become path" (Just Terra) (terrenoEmCelula (mapaJogo jogoEditado) 1 0)

posicaoEcra :: Posicao -> Maybe (Float, Float)
posicaoEcra = mapaParaEcra layoutTeste mapaTeste

layoutTeste :: MapLayoutConfig
layoutTeste = MapLayoutConfig 300 300 0

mapaTeste :: Mapa
mapaTeste =
  [ [Relva, Relva, Relva],
    [Terra, Asfalto, Terra],
    [Relva, Relva, Relva]
  ]

jogoTeste :: Jogo
jogoTeste =
  Jogo
    { baseJogo = Base 100 (2.5, 1.5) 100,
      portaisJogo = [Portal (0.5, 1.5) []],
      torresJogo = [],
      mapaJogo = mapaTeste,
      inimigosJogo = [],
      lojaJogo = []
    }
