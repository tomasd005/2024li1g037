module Desenhar where

import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Juicy
import ImmutableTowers
import LI12425
import Tarefa1 (terrenoPorPosicao)  -- ADICIONAR ESTA IMPORTAÇÃO
import Tarefa2 (inimigosNoAlcance)

-- ============================================================================
-- FUNÇÃO PRINCIPAL DE DESENHO
-- ============================================================================

desenha :: ImmutableTowers -> IO Picture
desenha e@(ImmutableTowers jogo imgs modo _ _) = case modo of
  MenuInicial opcao -> return $ desenhaMenu imgs opcao
  Pausado -> return $ desenhaPausado
  EmJogo -> return $ desenhaJogoCompleto e
  TutorialFoto -> return $ getImagem ImagemTutorial imgs
  MostrarCreditos -> return $ getImagem ImagemCreditos imgs

-- ============================================================================
-- DESENHO DO JOGO EM MODO EMJOGO - ATUALIZADO
-- ============================================================================

desenhaJogoCompleto :: ImmutableTowers -> Picture
desenhaJogoCompleto e =
  Pictures [
    -- Camadas de baixo para cima
    mapaToPicture (imagens e) (mapaJogo (jogo e)),
    desenhaTorres e,
    desenhaInimigosComEfeitos e,
    desenhaProjeteis e,
    desenhaPortais e,
    desenhaBase e (baseJogo (jogo e)),
    desenhaHUD e,
    desenhaLoja e,
    desenhaGameOver e,
    desenhaInstrucoes e  -- ADICIONADO
  ]

-- ============================================================================
-- DESENHO DE INSTRUÇÕES - NOVO
-- ============================================================================

desenhaInstrucoes :: ImmutableTowers -> Picture
desenhaInstrucoes e =
  case torreSelecionada e of
    Nothing -> 
      Translate (-larguraJanela/2 + 200) (alturaJanela/2 - 150) $
      Scale 0.15 0.15 $ Color white $ 
      Text "Clique nas torres abaixo para comprar"
    Just _ ->
      Translate (-larguraJanela/2 + 200) (alturaJanela/2 - 150) $
      Scale 0.15 0.15 $ Color yellow $ 
      Text "Clique na RELVA para colocar (Botao direito cancela)"

-- ============================================================================
-- DESENHO DO MENU
-- ============================================================================

desenhaMenu :: Imagens -> MenuInicialOpcoes -> Picture
desenhaMenu imgs opcaoSel =
  Pictures [
    getImagem Fundo imgs,
    botao Play (-400) opcaoSel Jogar,
    botao ButaoCreditos 0 opcaoSel Creditos,
    botao Exit 400 opcaoSel Sair,
    -- Instruções
    Translate 0 800 $ Scale 0.2 0.2 $ Color white $ Text "Use SETAS ou CLIQUE nos botoes"
  ]
  where
    botao img x sel alvo =
      let selecionado = sel == alvo
          escala = if selecionado then 0.35 else 0.25
          y = if selecionado then -800 else -850
          cor = if selecionado then yellow else white
          corFundo = if selecionado 
                     then makeColor 1 1 0 0.3  -- Amarelo translúcido
                     else makeColor 1 1 1 0.1  -- Branco translúcido
       in Pictures [
            -- Fundo do botão
            Color corFundo $ Translate x y $ rectangleSolid 300 80,
            -- Borda
            Color cor $ Translate x y $ rectangleWire 300 80,
            -- Imagem/texto do botão
            Translate x y $ Scale escala escala $ getImagem img imgs,
            -- Texto abaixo do botão
            Translate x (y - 60) $ Scale 0.15 0.15 $ Color white $ Text (
              case alvo of
                Jogar -> "JOGAR"
                Creditos -> "TUTORIAL"
                Sair -> "SAIR"
            )
          ]

-- ============================================================================
-- DESENHO DE TORRES
-- ============================================================================

desenhaTorres :: ImmutableTowers -> Picture
desenhaTorres e =
  Pictures $ concatMap (desenhaTorreCompleta e) (torresJogo (jogo e))

desenhaTorreCompleta :: ImmutableTowers -> Torre -> [Picture]
desenhaTorreCompleta e torre =
  let alcance = desenhaAlcanceTorre (mapaJogo (jogo e)) torre (torreSelecionada e)
      sprite = desenhaTorreSprite e torre
      cooldown = desenhaCooldownTorre torre
   in [alcance, sprite, cooldown]

desenhaAlcanceTorre :: Mapa -> Torre -> Maybe Torre -> Picture
desenhaAlcanceTorre mapa torre torreSel =
  let (x, y) = posicaoTorre torre
      raio = alcanceTorre torre * calculaTamanhoBloco mapa
      -- Mostra alcance só se torre estiver selecionada
      alpha = case torreSel of
                Just t | posicaoTorre t == posicaoTorre torre -> 0.4
                _ -> 0
   in Color (withAlpha alpha blue) $ 
        Translate (converteX (realToFrac x)) (converteY (realToFrac y)) $ 
        Circle raio

desenhaTorreSprite :: ImmutableTowers -> Torre -> Picture
desenhaTorreSprite e torre =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoTorre torre
      img = case projetilTorre torre of
        Projetil Resina _ -> TorreResina
        Projetil Gelo _ -> TorreGelo
        Projetil Fogo _ -> TorreFogo
      escala = escalaImagem bloco 500
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y)
   in Translate posX posY $ Scale escala escala $ getImagem img (imagens e)

-- Barra de cooldown acima da torre
desenhaCooldownTorre :: Torre -> Picture
desenhaCooldownTorre torre =
  let (x, y) = posicaoTorre torre
      progresso = max 0 (1 - (tempoTorre torre / cicloTorre torre))
      largura = 30 * progresso
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y) + 25
      corBarra = if progresso >= 1 then green else red
   in Translate posX posY $ Pictures [
        Color white $ rectangleWire 32 6,
        Color corBarra $ rectangleSolid largura 4
      ]

-- ============================================================================
-- DESENHO DE INIMIGOS COM EFEITOS VISUAIS
-- ============================================================================

desenhaInimigosComEfeitos :: ImmutableTowers -> Picture
desenhaInimigosComEfeitos e =
  Pictures $ map (desenhaInimigoComEfeitos e) (inimigosJogo (jogo e))

desenhaInimigoComEfeitos :: ImmutableTowers -> Inimigo -> Picture
desenhaInimigoComEfeitos e inimigo =
  let sprite = desenhaInimigoSprite e inimigo
      efeitos = desenhaEfeitosInimigo e inimigo
      vidaBar = desenhaBarraVida inimigo
   in Pictures [sprite, efeitos, vidaBar]

desenhaInimigoSprite :: ImmutableTowers -> Inimigo -> Picture
desenhaInimigoSprite e inimigo =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoInimigo inimigo
      escala = escalaImagem bloco 500
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y)
   in Translate posX posY $ Scale escala escala $ 
        getImagem Inimigocima (imagens e)

-- Efeitos visuais de projéteis ativos
desenhaEfeitosInimigo :: ImmutableTowers -> Inimigo -> Picture
desenhaEfeitosInimigo e inimigo =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoInimigo inimigo
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y)
      projeteis = projeteisInimigo inimigo
      
      -- Efeito de fogo (chamas)
      temFogo = any (\p -> tipoProjetil p == Fogo) projeteis
      efeitoFogo = if temFogo
        then Color (withAlpha 0.6 red) $ 
               Translate posX posY $ Circle (bloco/3)
        else Blank
      
      -- Efeito de gelo (cristais)
      temGelo = any (\p -> tipoProjetil p == Gelo) projeteis
      efeitoGelo = if temGelo
        then Color (withAlpha 0.7 cyan) $
               Translate posX posY $ Pictures [
                 rectangleSolid (bloco/2) 2,
                 Rotate 45 $ rectangleSolid (bloco/2) 2
               ]
        else Blank
      
      -- Efeito de resina (gotas)
      temResina = any (\p -> tipoProjetil p == Resina) projeteis
      efeitoResina = if temResina
        then Color (withAlpha 0.5 (makeColor 0.6 0.4 0.2 1.0)) $
               Translate posX posY $ Circle (bloco/4)
        else Blank
   
   in Pictures [efeitoFogo, efeitoGelo, efeitoResina]

-- Barra de vida acima do inimigo
desenhaBarraVida :: Inimigo -> Picture
desenhaBarraVida inimigo =
  let (x, y) = posicaoInimigo inimigo
      -- Assume vida máxima de 100 (ajustar se necessário)
      vidaMax = 100.0
      percentualVida = max 0 (min 1 (vidaInimigo inimigo / vidaMax))
      larguraTotal = 30
      larguraVida = larguraTotal * percentualVida
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y) - 25
      corVida = if percentualVida > 0.5 then green
                else if percentualVida > 0.25 then yellow
                else red
   in Translate posX posY $ Pictures [
        Color white $ rectangleWire (larguraTotal + 2) 6,
        Translate (-(larguraTotal - larguraVida)/2) 0 $
          Color corVida $ rectangleSolid larguraVida 4
      ]

-- ============================================================================
-- DESENHO DE PROJÉTEIS EM VOO
-- ============================================================================

desenhaProjeteis :: ImmutableTowers -> Picture
desenhaProjeteis e =
  Pictures $ concatMap (desenhaProjeteisdeTorre e) (torresJogo (jogo e))

desenhaProjeteisdeTorre :: ImmutableTowers -> Torre -> [Picture]
desenhaProjeteisdeTorre e torre =
  -- Só desenha projéteis se torre acabou de disparar (cooldown alto)
  if tempoTorre torre > cicloTorre torre * 0.8
  then
    let inimigosAlvo = take (rajadaTorre torre) (inimigosNoAlcance torre (inimigosJogo (jogo e)))
     in map (desenhaProjetil torre) inimigosAlvo
  else []

desenhaProjetil :: Torre -> Inimigo -> Picture
desenhaProjetil torre inimigo =
  let (x1, y1) = posicaoTorre torre
      (x2, y2) = posicaoInimigo inimigo
      cor = case projetilTorre torre of
        Projetil Fogo _ -> red
        Projetil Gelo _ -> cyan
        Projetil Resina _ -> makeColor 0.6 0.4 0.2 1.0
   in Color cor $ Line [
        (converteX (realToFrac x1), converteY (realToFrac y1)),
        (converteX (realToFrac x2), converteY (realToFrac y2))
      ]

-- ============================================================================
-- DESENHO DE PORTAIS
-- ============================================================================

desenhaPortais :: ImmutableTowers -> Picture
desenhaPortais e =
  Pictures $ map (desenhaPortal e) (portaisJogo (jogo e))

desenhaPortal :: ImmutableTowers -> Portal -> Picture
desenhaPortal e portal =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoPortal portal
      escala = escalaImagem bloco 500
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y)
   in Translate posX posY $ Scale escala escala $ 
        getImagem PortalFoto (imagens e)

-- ============================================================================
-- DESENHO DA BASE
-- ============================================================================

desenhaBase :: ImmutableTowers -> Base -> Picture
desenhaBase e base =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoBase base
      escalaX = escalaImagem bloco 471
      escalaY = escalaImagem bloco 530
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y)
   in Translate posX posY $ Scale escalaX escalaY $ 
        getImagem BaseFoto (imagens e)

-- ============================================================================
-- HUD (HEADS-UP DISPLAY)
-- ============================================================================

desenhaHUD :: ImmutableTowers -> Picture
desenhaHUD e =
  let base = baseJogo (jogo e)
      vida = floor (vidaBase base) :: Int
      creditos = creditosBase base
      
      -- Painel semi-transparente
      painel = Color (withAlpha 0.7 black) $
                 Translate 0 (alturaJanela/2 - 60) $
                 rectangleSolid larguraJanela 100
      
      -- Vida
      textoVida = Translate (-larguraJanela/2 + 100) (alturaJanela/2 - 50) $
                    Pictures [
                      Color white $ Scale 0.2 0.2 $ Text "Vida:",
                      Translate 150 0 $ Color red $ Scale 0.25 0.25 $ 
                        Text (show vida)
                    ]
      
      -- Créditos
      textoCreditos = Translate (-larguraJanela/2 + 100) (alturaJanela/2 - 90) $
                        Pictures [
                          Color white $ Scale 0.2 0.2 $ Text "Creditos:",
                          Translate 150 0 $ Color yellow $ Scale 0.25 0.25 $ 
                            Text (show creditos)
                        ]
      
      -- Ondas restantes
      totalOndas = sum $ map (length . inimigosOnda) $ concatMap ondasPortal $ portaisJogo (jogo e)
      inimigosAtivos = length (inimigosJogo (jogo e))
      textoInimigos = Translate (larguraJanela/2 - 300) (alturaJanela/2 - 70) $
                        Pictures [
                          Color white $ Scale 0.2 0.2 $ Text "Inimigos:",
                          Translate 200 0 $ Color orange $ Scale 0.25 0.25 $
                            Text (show inimigosAtivos ++ " / " ++ show totalOndas)
                        ]
   
   in Pictures [painel, textoVida, textoCreditos, textoInimigos]

-- ============================================================================
-- LOJA (SHOP) - ATUALIZADA
-- ============================================================================

desenhaLoja :: ImmutableTowers -> Picture
desenhaLoja e =
  let loja = lojaJogo (jogo e)
      bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      torreSel = torreSelecionada e
   in Pictures $ zipWith (desenhaBotaoLoja bloco torreSel (imagens e)) [0..] loja

desenhaBotaoLoja :: Float -> Maybe Torre -> Imagens -> Int -> (Creditos, Torre) -> Picture
desenhaBotaoLoja bloco torreSel imgs indice (preco, torre) =
  let posX = -larguraJanela/2 + 150 + fromIntegral indice * 160
      posY = -alturaJanela/2 + 100
      selecionada = case torreSel of
                      Just t -> tipoProjetil (projetilTorre t) == 
                                tipoProjetil (projetilTorre torre)
                      Nothing -> False
      cor = if selecionada then green else white
      corFundo = if selecionada 
                 then makeColor 0 0.5 0 0.8
                 else makeColor 0.2 0.2 0.2 0.8
      
      img = case projetilTorre torre of
        Projetil Resina _ -> TorreResina
        Projetil Gelo _ -> TorreGelo
        Projetil Fogo _ -> TorreFogo
      
      -- Adiciona informações da torre
      textoInfo = if selecionada 
                  then Pictures [
                    Translate 0 50 $ Scale 0.08 0.08 $ Color white $ 
                      Text ("Alc: " ++ show (floor $ alcanceTorre torre :: Int)),
                    Translate 0 35 $ Scale 0.08 0.08 $ Color white $ 
                      Text ("Dano: " ++ show (floor $ danoTorre torre :: Int))
                  ]
                  else Blank
   
   in Translate posX posY $ Pictures [
        -- Fundo
        Color corFundo $ rectangleSolid 140 140,
        -- Borda (mais grossa se selecionada)
        Color cor $ 
          if selecionada 
          then Pictures [rectangleWire 142 142, rectangleWire 144 144, rectangleWire 146 146]
          else rectangleWire 142 142,
        -- Imagem da torre
        Scale 0.15 0.15 $ getImagem img imgs,
        -- Preço
        Translate 0 (-80) $ Scale 0.15 0.15 $ Color yellow $ Text (show preco),
        -- Informações
        textoInfo
      ]

-- ============================================================================
-- GAME OVER (VITÓRIA/DERROTA)
-- ============================================================================

desenhaGameOver :: ImmutableTowers -> Picture
desenhaGameOver e =
  let base = baseJogo (jogo e)
      inimigos = inimigosJogo (jogo e)
      ondasRestantes = concatMap ondasPortal (portaisJogo (jogo e))
      
      ganhou = vidaBase base > 0 && null inimigos && all (null . inimigosOnda) ondasRestantes
      perdeu = vidaBase base <= 0
   
   in if ganhou then desenhaVitoria
      else if perdeu then desenhaDerrota
      else Blank

desenhaVitoria :: Picture
desenhaVitoria =
  Pictures [
    Color (withAlpha 0.9 black) $ rectangleSolid larguraJanela alturaJanela,
    Color green $ Scale 0.6 0.6 $ Translate (-350) 200 $ Text "VITORIA!",
    Color white $ Scale 0.25 0.25 $ Translate (-450) 0 $ Text "Parabens! Voce defendeu a base!",
    Color yellow $ Scale 0.2 0.2 $ Translate (-400) (-100) $ Text "Pressione ESC para sair"
  ]

desenhaDerrota :: Picture
desenhaDerrota =
  Pictures [
    Color (withAlpha 0.9 black) $ rectangleSolid larguraJanela alturaJanela,
    Color red $ Scale 0.6 0.6 $ Translate (-400) 200 $ Text "DERROTA!",
    Color white $ Scale 0.25 0.25 $ Translate (-450) 0 $ Text "A base foi destruida...",
    Color yellow $ Scale 0.2 0.2 $ Translate (-400) (-100) $ Text "Pressione ESC para sair"
  ]

desenhaPausado :: Picture
desenhaPausado =
  Pictures [
    Color (withAlpha 0.7 black) $ rectangleSolid larguraJanela alturaJanela,
    Color yellow $ Scale 0.4 0.4 $ Translate (-300) 0 $ Text "PAUSADO",
    Color white $ Scale 0.2 0.2 $ Translate (-400) (-150) $ Text "Pressione P para continuar"
  ]

-- ============================================================================
-- FUNÇÕES AUXILIARES
-- ============================================================================

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

mapaToPicture :: Imagens -> Mapa -> Picture
mapaToPicture imgs terreno =
  let bloco = calculaTamanhoBloco terreno
      largura = fromIntegral (length (head terreno))
      altura = fromIntegral (length terreno)
      offsetX = -(largura * bloco) / 2
      offsetY = -(altura * bloco) / 2
   in Pictures [
        Translate
          (offsetX + fromIntegral x * bloco + bloco / 2)
          (offsetY + fromIntegral y * bloco + bloco / 2)
          $ blocoToPicture imgs b
        | (y, linha) <- zip [0..] (reverse terreno),
          (x, b) <- zip [0..] linha
      ]

calculaTamanhoBloco :: Mapa -> Float
calculaTamanhoBloco terreno =
  let largura = fromIntegral (length (head terreno))
      altura = fromIntegral (length terreno)
   in min (larguraJanela / largura) (alturaJanela / altura)

converteX :: Double -> Float
converteX x = (-larguraJanela / 2) + (double2Float x * int2Float pixeis)

converteY :: Double -> Float
converteY y = (alturaJanela / 2) - (double2Float y * int2Float pixeis)

escalaImagem :: Float -> Float -> Float
escalaImagem bloco tamanhoOriginal = bloco / tamanhoOriginal

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
  play <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/play.bmp"
  exit <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/exit.bmp"
  creditos <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/creditos.bmp"
  tutorial <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/tutorial.bmp"

  let imgs = [
        (Fundo, fundo), (Grass, grass), (Water, water), (Land, land),
        (TorreResina, torreResina), (TorreGelo, torreGelo), (TorreFogo, torreFogo),
        (Inimigocima, inimigo), (BaseFoto, base), (PortalFoto, portal),
        (Play, play), (Exit, exit), (ButaoCreditos, creditos),
        (ImagemTutorial, tutorial), (ImagemCreditos, creditos)
        ]
      
      jogoInicial = Jogo {
        mapaJogo = mapa01,
        baseJogo = base01,
        portaisJogo = [portal01],
        torresJogo = [],
        inimigosJogo = [],
        lojaJogo = [
          (50, Torre (0, 0) 20 4 2 2 5 (Projetil Resina (Finita 3))),
          (50, Torre (0, 0) 20 4 2 2 5 (Projetil Gelo (Finita 2))),
          (50, Torre (0, 0) 20 4 2 2 5 (Projetil Fogo (Finita 1)))
        ]
      }
  
  return $ ImmutableTowers jogoInicial imgs (MenuInicial Jogar) 0 Nothing