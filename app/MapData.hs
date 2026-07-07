module MapData
  ( mapa01,
    mapa02,
    base01,
    portalBase,
  )
where

import LI12425

mapa01 :: [[Terreno]]
mapa01 =
  [ [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, a, a, a, a, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, a, a, a, a, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, a, a, a, a, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, a, a, a, a, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, a, a, a, a, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r]
  ]
  where
    t = Terra
    r = Relva
    a = Agua

mapa02 :: [[Terreno]]
mapa02 = map (map transforma) mapa01
  where
    transforma Terra = Asfalto
    transforma terreno = terreno

base01 :: Base
base01 = Base {posicaoBase = (35.93347, 22.000067), creditosBase = 150, vidaBase = 80}

portalBase :: Portal
portalBase = Portal {ondasPortal = [], posicaoPortal = (0, 2)}
