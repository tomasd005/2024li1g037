module TowerSystem
  ( torreResinaBase,
    torreGeloBase,
    torreFogoBase,
    torreImpactoBase,
    torreMedoBase,
    torreVenenoBase,
    torreEletricaBase,
    lojaParaModo,
    upgradeTorre,
    custoUpgradeTorre,
    valorVendaTorre,
    mesmoModeloTorre,
  )
where

import ImmutableTowers (ModoJogoEscolhido (..))
import LI12425

torreResinaBase, torreGeloBase, torreFogoBase, torreImpactoBase, torreMedoBase, torreVenenoBase, torreEletricaBase :: Torre
torreResinaBase = Torre (0, 0) 15 5.2 1 1.2 0 (Projetil Resina (Finita 3.5))
torreGeloBase = Torre (0, 0) 9 4.3 2 1.8 0 (Projetil Gelo (Finita 1.5))
torreFogoBase = Torre (0, 0) 27 3.8 1 0.95 0 (Projetil Fogo (Finita 2.2))
torreImpactoBase = Torre (0, 0) 42 3.1 1 1.75 0 (Projetil Fogo (Finita 0.6))
torreMedoBase = Torre (0, 0) 6 4.6 1 2.0 0 (Projetil Medo (Finita 1.8))
torreVenenoBase = Torre (0, 0) 8 4.8 2 1.55 0 (Projetil Veneno (Finita 4.2))
torreEletricaBase = Torre (0, 0) 18 4.0 3 2.35 0 (Projetil Eletrico (Finita 0.9))

lojaParaModo :: ModoJogoEscolhido -> Loja
lojaParaModo modoAtual =
  let desconto = case modoAtual of
        ModoSandbox -> 25
        ModoDesafio -> 15
        _ -> 0
      preco p = max 10 (p - desconto)
   in [ (preco 45, torreResinaBase),
        (preco 60, torreGeloBase),
        (preco 75, torreFogoBase),
        (preco 95, torreMedoBase),
        (preco 105, torreVenenoBase),
        (preco 120, torreEletricaBase),
        (preco 140, torreImpactoBase)
      ]

upgradeTorre :: Torre -> Torre
upgradeTorre torre =
  torre
    { danoTorre = danoTorre torre * 1.32 + 4,
      alcanceTorre = alcanceTorre torre + 0.32,
      rajadaTorre = if danoTorre torre > 45 then min 5 (rajadaTorre torre + 1) else rajadaTorre torre,
      cicloTorre = max 0.42 (cicloTorre torre * 0.9),
      tempoTorre = min (tempoTorre torre) (max 0.42 (cicloTorre torre * 0.9)),
      projetilTorre = melhoraProjetil (projetilTorre torre)
    }

melhoraProjetil :: Projetil -> Projetil
melhoraProjetil projetil = projetil {duracaoProjetil = melhoraDuracao (duracaoProjetil projetil)}
  where
    melhoraDuracao Infinita = Infinita
    melhoraDuracao (Finita t) = Finita (t * 1.18 + 0.25)

custoUpgradeTorre :: Torre -> Creditos
custoUpgradeTorre torre =
  floor (30 + danoTorre torre * 2.2 + alcanceTorre torre * 11 + fromIntegral (rajadaTorre torre * 24) + (2.1 - min 2.0 (cicloTorre torre)) * 45)

valorVendaTorre :: Torre -> Creditos
valorVendaTorre torre =
  max 15 (floor (fromIntegral (custoUpgradeTorre torre) * (0.55 :: Float)))

mesmoModeloTorre :: Torre -> Torre -> Bool
mesmoModeloTorre t1 t2 =
  tipoProjetil (projetilTorre t1) == tipoProjetil (projetilTorre t2)
    && danoTorre t1 == danoTorre t2
    && alcanceTorre t1 == alcanceTorre t2
    && cicloTorre t1 == cicloTorre t2
    && rajadaTorre t1 == rajadaTorre t2
