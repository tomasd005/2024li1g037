module Desenhar where

import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Juicy
import ImmutableTowers
import LI12425

-- | Função principal de desenho, varia conforme o modo de jogo
desenha :: ImmutableTowers -> IO Picture
desenha e@(ImmutableTowers jogo imgs modo _ _) = case modo of
  MenuInicial opcao -> return $ desenhaMenu imgs opcao
  Pausado -> return $ Translate (-200) 0 $ Scale 0.3 0.3 $ Color white $ Text "Jogo Pausado"
  EmJogo ->
    return $
      Pictures
        [ mapaToPicture imgs (mapaJogo jogo),
          desenhaTorres e,
          desenhaInimigos e,
          desenhaPortal e (head (portaisJogo jogo)),
          desenhaBase e (baseJogo jogo)
        ]
  TutorialFoto -> return $ getImagem ImagemTutorial imgs
  MostrarCreditos -> return $ getImagem ImagemCreditos imgs

-- | Desenha o menu inicial
desenhaMenu :: Imagens -> MenuInicialOpcoes -> Picture
desenhaMenu imgs opcaoSel =
  Pictures
    [ getImagem Fundo imgs,
      botao Play (-1000) opcaoSel Jogar,
      botao ButaoCreditos 0 opcaoSel Creditos,
      botao Exit 1000 opcaoSel Sair
    ]
  where
    botao img x sel alvo =
      let s = if sel == alvo then 0.3 else 0.2
          y = if sel == alvo then -950 else -1400
       in Scale s s (Translate x y (getImagem img imgs))

-- | Desenha todas as torres com o seu alcance
desenhaTorres :: ImmutableTowers -> Picture
desenhaTorres e =
  Pictures $ concatMap (\torre -> [desenhaAlcanceTorre (mapaJogo (jogo e)) torre, desenhaTorre e torre]) (torresJogo (jogo e))

desenhaTorre :: ImmutableTowers -> Torre -> Picture
desenhaTorre e torre =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoTorre torre
      img = case projetilTorre torre of
        Projetil Resina _ -> TorreResina
        Projetil Gelo _ -> TorreGelo
        Projetil Fogo _ -> TorreFogo
      escala = escalaImagem bloco 500 -- 500x500 torre
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y)
   in Translate posX posY (Scale escala escala (getImagem img (imagens e)))

desenhaAlcanceTorre :: Mapa -> Torre -> Picture
desenhaAlcanceTorre mapa torre =
  let (x, y) = posicaoTorre torre
      raio = alcanceTorre torre * calculaTamanhoBloco mapa
   in Color (withAlpha 0.3 blue) $ Translate (converteX (realToFrac x)) (converteY (realToFrac y)) $ Circle raio

-- | Desenha todos os inimigos
desenhaInimigos :: ImmutableTowers -> Picture
desenhaInimigos e =
  Pictures $ map (desenhaInimigo e) (inimigosJogo (jogo e))

desenhaInimigo :: ImmutableTowers -> Inimigo -> Picture
desenhaInimigo e inimigo =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoInimigo inimigo
      escala = escalaImagem bloco 500 -- 500x500 inimigo
   in Translate (converteX (realToFrac x)) (converteY (realToFrac y)) (Scale escala escala (getImagem Inimigocima (imagens e)))

desenhaPortal :: ImmutableTowers -> Portal -> Picture
desenhaPortal e portal =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoPortal portal
      escala = escalaImagem bloco 500 -- 500x500 portal
   in Translate (converteX (realToFrac x)) (converteY (realToFrac y)) (Scale escala escala (getImagem PortalFoto (imagens e)))

desenhaBase :: ImmutableTowers -> Base -> Picture
desenhaBase e base =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoBase base
      escalaX = escalaImagem bloco 471 -- base largura
      escalaY = escalaImagem bloco 530 -- base altura
   in Translate (converteX (realToFrac x)) (converteY (realToFrac y)) (Scale escalaX escalaY (getImagem BaseFoto (imagens e)))

blocoToPicture :: Imagens -> Terreno -> Picture
blocoToPicture imgs t =
  let bloco = calculaTamanhoBloco mapa01
      (img, w, h) = case t of
        Relva -> (Grass, 225, 225)
        Agua -> (Water, 1366, 768)
        Terra -> (Land, 980, 980)
      escalaX = escalaImagem bloco w
      escalaY = escalaImagem bloco h
   in Scale escalaX escalaY (getImagem img imgs)

-- | Desenha o mapa completo
mapaToPicture :: Imagens -> Mapa -> Picture
mapaToPicture imgs terreno =
  let bloco = calculaTamanhoBloco terreno
      largura = fromIntegral (length (head terreno))
      altura = fromIntegral (length terreno)
      offsetX = -(largura * bloco) / 2
      offsetY = -(altura * bloco) / 2
   in Pictures
        [ Translate
            (offsetX + fromIntegral x * bloco + bloco / 2)
            (offsetY + fromIntegral y * bloco + bloco / 2)
            $ blocoToPicture imgs b
          | (y, linha) <- zip [0 ..] (reverse terreno), -- invertendo linhas
            (x, b) <- zip [0 ..] linha
        ]

-- | Calcula o tamanho de cada bloco
calculaTamanhoBloco :: Mapa -> Float
calculaTamanhoBloco terreno =
  let largura = fromIntegral (length (head terreno))
      altura = fromIntegral (length terreno)
   in min (larguraJanela / largura) (alturaJanela / altura)

-- | Converte posição X para coordenada Gloss
converteX :: Double -> Float
converteX x = (-larguraJanela / 2) + (double2Float x * int2Float pixeis)

-- | Converte posição Y para coordenada Gloss
converteY :: Double -> Float
converteY y = (alturaJanela / 2) - (double2Float y * int2Float pixeis)

-- | Função que carrega as imagens do jogo
carregarImagens :: IO ImmutableTowers
carregarImagens = do
  fundo <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/fundo_1_.bmp"
  grass <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/images.bmp"
  water <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/6da00a37f26551f688dcc04367d7c73c_1.bmp"
  land <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/terra_textura.bmp"
  torreResina <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp"
  torreGelo <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp"
  torreFogo <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp"
  inimigo <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/enemy-clipart-little-monster-holding-a-gun-in-one-hand_546721_wh860_2_-removebg-preview.bmp"
  base <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/tower_image-removebg-preview.bmp"
  portal <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/portal.bmp"

  let imgs =
        [ (Fundo, fundo),
          (Grass, grass),
          (Water, water),
          (Land, land),
          (TorreResina, torreResina),
          (TorreGelo, torreGelo),
          (TorreFogo, torreFogo),
          (Inimigocima, inimigo),
          (BaseFoto, base),
          (PortalFoto, portal)
        ]
      jogoInicial =
        Jogo
          { mapaJogo = mapa01,
            baseJogo = base01,
            portaisJogo = [portal01],
            torresJogo = [],
            inimigosJogo = [],
            lojaJogo =
              [ (50, Torre (0, 0) 20 4 2 2 5 (Projetil Resina (Finita 3))),
                (50, Torre (0, 0) 20 4 2 2 5 (Projetil Gelo (Finita 2))),
                (50, Torre (0, 0) 20 4 2 2 5 (Projetil Fogo (Finita 1)))
              ]
          }
  return $ ImmutableTowers jogoInicial imgs (MenuInicial Jogar) 0 Nothing

escalaImagem :: Float -> Float -> Float
escalaImagem bloco tamanhoOriginal = bloco / tamanhoOriginal


