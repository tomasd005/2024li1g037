module WaveSystemSpec (testesWaveSystem) where

import LI12425
import Test.HUnit
import WaveSystem

testesWaveSystem :: Test
testesWaveSystem =
  TestLabel "Wave plans and mutators" $
    test
      [ "infinite mutators appear at deterministic milestones" ~: marcosMutadores,
        "double wave duplicates its composition" ~: mutadorOndaDupla,
        "fortified waves increase health without invalid timing" ~: mutadorFortificado,
        "generated waves keep safe spawn cycles" ~: ciclosSeguros
      ]

marcosMutadores :: Assertion
marcosMutadores = do
  mutadoresOndaInfinita 4 @?= [Fortificados]
  mutadoresOndaInfinita 6 @?= [RecompensaEscassa]
  mutadoresOndaInfinita 18 @?= [RecompensaEscassa, OndaDupla]

mutadorOndaDupla :: Assertion
mutadorOndaDupla =
  let onda = criaOnda 3 8 0
      alterada = aplicaMutadoresInfinito 9 onda
   in length (inimigosOnda alterada) @?= length (inimigosOnda onda) * 2

mutadorFortificado :: Assertion
mutadorFortificado =
  let onda = criaOnda 4 8 0
      alterada = aplicaMutadoresInfinito 4 onda
   in case (inimigosOnda onda, inimigosOnda alterada) of
        (original : _, fortalecido : _) -> do
          assertBool "Fortified enemy should have more health" (vidaInimigo fortalecido > vidaInimigo original)
          assertBool "Mutator must preserve a positive spawn cycle" (cicloOnda alterada > 0)
        _ -> assertFailure "Expected a non-empty generated wave"

ciclosSeguros :: Assertion
ciclosSeguros =
  assertBool "Every representative wave should have a positive cycle" $
    all ((> 0) . cicloOnda) [criaOnda nivel (5 + nivel) 0 | nivel <- [1 .. 80]]
