module Desenhar where

import GHC.Float (double2Float)
import Graphics.Gloss
import Graphics.Gloss.Juicy
import ImmutableTowers
import LI12425
import Tarefa2 (inimigosNoAlcance)

-- Paleta mais sóbria, inspirada em tower defenses com tabuleiro tático:
-- terreno escuro, água dessaturada, UI em painéis translúcidos.
mapaLarguraMax, mapaAlturaMax, mapaCentroY :: Float
mapaLarguraMax = 900
mapaAlturaMax = 760
mapaCentroY = 20

corFundoJogo, corPainel, corTextoSuave :: Color
corFundoJogo = makeColorI 13 17 15 255
corPainel = makeColorI 24 31 26 225
corTextoSuave = makeColorI 220 226 214 255

corRelvaBase, corRelvaAlt, corTerraBase, corAguaBase :: Color
corRelvaBase = makeColorI 62 86 49 255
corRelvaAlt = makeColorI 72 98 58 255
corTerraBase = makeColorI 86 65 43 255
corAguaBase = makeColorI 58 103 122 255

-- ============================================================================
-- FUNÇÃO PRINCIPAL DE DESENHO
-- ============================================================================

desenha :: ImmutableTowers -> IO Picture
desenha e@(ImmutableTowers _ imgs modo _ _ _) = case modo of
  MenuInicial opcao -> return $ desenhaMenu imgs opcao
  Pausado -> return $ desenhaPausado
  EmJogo -> return $ desenhaJogoCompleto e
  TutorialFoto -> return desenhaTutorial
  MostrarCreditos -> return desenhaCreditos

-- ============================================================================
-- DESENHO DO JOGO EM MODO EMJOGO - ATUALIZADO
-- ============================================================================

desenhaJogoCompleto :: ImmutableTowers -> Picture
desenhaJogoCompleto e =
  Pictures [
    Color corFundoJogo $ rectangleSolid larguraJanela alturaJanela,
    -- Camadas de baixo para cima
    mapaToPicture (imagens e) (mapaJogo (jogo e)),
    desenhaPreVisualizacaoColocacao e,
    desenhaTorres e,
    desenhaInimigosComEfeitos e,
    desenhaProjeteis e,
    desenhaPortais e,
    desenhaBase e (baseJogo (jogo e)),
    desenhaHUD e,
    desenhaPainelLateral e,
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
      Translate (-larguraJanela/2 + 44) (alturaJanela/2 - 128) $
      Scale 0.12 0.12 $ Color corTextoSuave $ 
      Text "Clique nas torres abaixo para comprar"
    Just _ ->
      Translate (-larguraJanela/2 + 44) (alturaJanela/2 - 128) $
      Scale 0.12 0.12 $ Color (makeColorI 235 194 96 255) $ 
      Text "Clique na RELVA para colocar (Botao direito cancela)"

-- ============================================================================
-- DESENHO DO MENU
-- ============================================================================

desenhaMenu :: Imagens -> MenuInicialOpcoes -> Picture
desenhaMenu _ opcaoSel =
  Pictures [
    Color corFundoJogo $ rectangleSolid larguraJanela alturaJanela,
    Color (withAlpha 0.35 (makeColorI 40 52 43 255)) $ Translate 0 70 $ rectangleSolid 860 520,
    Color (makeColorI 68 78 63 255) $ Translate 0 70 $ rectangleWire 860 520,
    Translate (-300) 230 $ Scale 0.42 0.42 $ Color corTextoSuave $ Text "IMMUTABLE",
    Translate (-228) 170 $ Scale 0.42 0.42 $ Color (makeColorI 226 194 95 255) $ Text "TOWERS",
    Translate (-260) 108 $ Scale 0.105 0.105 $ Color (makeColorI 154 164 146 255) $
      Text "Tower defense tatico em grelha — protege a base, gere creditos e posiciona torres.",
    botao (-300) opcaoSel Jogar "JOGAR" "Comecar defesa",
    botao 0 opcaoSel Creditos "TUTORIAL" "Como jogar",
    botao 300 opcaoSel Sair "SAIR" "Fechar jogo",
    Translate (-205) (-370) $ Scale 0.105 0.105 $ Color (makeColorI 154 164 146 255) $
      Text "Setas/Enter ou clique nos botoes"
  ]
  where
    botao x sel alvo titulo subtitulo =
      let selecionado = sel == alvo
          y = -120
          corBorda = if selecionado then makeColorI 226 194 95 255 else makeColorI 92 103 88 255
          corFundo = if selecionado then makeColorI 55 60 45 235 else makeColorI 31 39 33 225
          marcador = if selecionado
                     then Color (withAlpha 0.35 (makeColorI 226 194 95 255)) $ Translate x (y + 58) $ rectangleSolid 210 6
                     else Blank
       in Pictures [
            Color corFundo $ Translate x y $ rectangleSolid 240 118,
            Color corBorda $ Translate x y $ rectangleWire 240 118,
            marcador,
            Translate (x - 76) (y + 12) $ Scale 0.16 0.16 $ Color corTextoSuave $ Text titulo,
            Translate (x - 76) (y - 28) $ Scale 0.075 0.075 $ Color (makeColorI 154 164 146 255) $ Text subtitulo
          ]

desenhaTutorial :: Picture
desenhaTutorial = desenhaPainelTexto "TUTORIAL" [
    "1. Compra uma torre na loja no canto inferior esquerdo.",
    "2. Move o rato sobre a relva para ver a celula, alcance e validade.",
    "3. Clique esquerdo coloca a torre; botao direito cancela a selecao.",
    "4. Torres so podem ser colocadas em relva livre, nunca no caminho/agua.",
    "5. Pressiona P para pausar. ESC volta ao menu neste ecra."
  ]

desenhaCreditos :: Picture
desenhaCreditos = desenhaPainelTexto "CREDITOS" [
    "Immutable Towers",
    "Projeto LI1 — Tower Defense em Haskell + Gloss.",
    "UI, feedback de colocacao e compatibilidade Windows/GHCup atualizados.",
    "Pressiona ENTER ou ESC para voltar."
  ]

desenhaPainelTexto :: String -> [String] -> Picture
desenhaPainelTexto titulo linhas =
  Pictures [
    Color corFundoJogo $ rectangleSolid larguraJanela alturaJanela,
    Color corPainel $ rectangleSolid 820 560,
    Color (makeColorI 68 78 63 255) $ rectangleWire 820 560,
    Translate (-330) 205 $ Scale 0.32 0.32 $ Color (makeColorI 226 194 95 255) $ Text titulo,
    Pictures [
      Translate (-330) (105 - fromIntegral i * 58) $ Scale 0.12 0.12 $ Color corTextoSuave $ Text linha
      | (i, linha) <- zip [0 :: Int ..] linhas
    ],
    Translate (-160) (-230) $ Scale 0.1 0.1 $ Color (makeColorI 154 164 146 255) $ Text "ESC / ENTER para voltar"
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
   in Color (withAlpha alpha (makeColorI 111 150 168 255)) $ 
        Translate (converteX (realToFrac x)) (converteY (realToFrac y)) $ 
        ThickCircle raio 2

desenhaPreVisualizacaoColocacao :: ImmutableTowers -> Picture
desenhaPreVisualizacaoColocacao e =
  case (torreSelecionada e, posicaoRato e) of
    (Just torre, Just rato) ->
      case ratoParaCelula (mapaJogo (jogo e)) rato of
        Just (cx, cy, centroX, centroY) ->
          let mapa = mapaJogo (jogo e)
              bloco = calculaTamanhoBloco mapa
              posCentro = (fromIntegral cx + 0.5, fromIntegral cy + 0.5)
              terrenoValido = terrenoEm cx cy mapa == Just Relva
              livre = notElem posCentro (map posicaoTorre (torresJogo (jogo e)))
              valido = terrenoValido && livre
              cor = if valido then makeColorI 124 171 102 255 else makeColorI 190 82 72 255
              torrePreview = torre {posicaoTorre = posCentro}
              alcance = alcanceTorre torrePreview * bloco
           in Pictures [
                Color (withAlpha 0.16 cor) $ Translate centroX centroY $ rectangleSolid bloco bloco,
                Color (withAlpha 0.95 cor) $ Translate centroX centroY $ rectangleWire bloco bloco,
                Color (withAlpha 0.18 (makeColorI 111 150 168 255)) $ Translate centroX centroY $ circleSolid alcance,
                Color (withAlpha 0.55 corTextoSuave) $ Translate centroX centroY $ modeloTorre bloco (projetilTorre torre) True,
                if valido
                  then Blank
                  else Color (withAlpha 0.85 cor) $ Translate centroX centroY $ Pictures [
                    Line [(-bloco*0.28, -bloco*0.28), (bloco*0.28, bloco*0.28)],
                    Line [(-bloco*0.28, bloco*0.28), (bloco*0.28, -bloco*0.28)]
                    ]
              ]
        Nothing -> Blank
    _ -> Blank

desenhaTorreSprite :: ImmutableTowers -> Torre -> Picture
desenhaTorreSprite e torre =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoTorre torre
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y)
   in Translate posX posY $ modeloTorre bloco (projetilTorre torre) False

modeloTorre :: Float -> Projetil -> Bool -> Picture
modeloTorre bloco projetil selecionada =
  let corpo = makeColorI 83 88 84 255
      sombra = makeColorI 20 24 22 170
      metalClaro = makeColorI 178 184 176 255
      acento = case tipoProjetil projetil of
        Resina -> makeColorI 145 107 61 255
        Gelo -> makeColorI 111 150 168 255
        Fogo -> makeColorI 175 88 58 255
      escala = bloco / 28
      aro = if selecionada
            then Color (makeColorI 226 194 95 255) $ rectangleWire (bloco * 0.92) (bloco * 0.92)
            else Blank
   in Pictures [
        Color sombra $ Translate 2 (-3) $ ThickCircle (bloco * 0.18) (bloco * 0.38),
        aro,
        Scale escala escala $ Pictures [
          Color corpo $ rectangleSolid 13 20,
          Color metalClaro $ Translate 0 10 $ rectangleSolid 18 5,
          Color metalClaro $ Translate 0 (-10) $ rectangleSolid 18 5,
          Color acento $ Translate 0 0 $ rectangleSolid 5 18,
          Color (withAlpha 0.35 black) $ Translate 5 0 $ rectangleSolid 3 18
        ]
      ]

-- Barra de cooldown acima da torre
desenhaCooldownTorre :: Torre -> Picture
desenhaCooldownTorre torre =
  let (x, y) = posicaoTorre torre
      progresso = max 0 (1 - (tempoTorre torre / cicloTorre torre))
      largura = 30 * progresso
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y) + 25
      corBarra = if progresso >= 1 then makeColorI 135 174 111 255 else makeColorI 190 82 72 255
   in Translate posX posY $ Pictures [
        Color corTextoSuave $ rectangleWire 32 6,
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
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y)
   in Translate posX posY $ modeloInimigo bloco

modeloInimigo :: Float -> Picture
modeloInimigo bloco =
  let pele = makeColorI 114 86 70 255
      contorno = makeColorI 39 31 29 255
      olho = makeColorI 222 198 118 255
      escala = bloco / 24
   in Scale escala escala $ Pictures [
        Color (withAlpha 0.35 black) $ Translate 2 (-3) $ circleSolid 10,
        Color contorno $ circleSolid 10,
        Color pele $ circleSolid 8,
        Color olho $ Translate (-3) 2 $ circleSolid 1.6,
        Color olho $ Translate 3 2 $ circleSolid 1.6,
        Color contorno $ Translate 0 (-3) $ rectangleSolid 8 2
      ]

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
        then Color (withAlpha 0.55 (makeColorI 175 88 58 255)) $ 
               Translate posX posY $ Circle (bloco/3)
        else Blank
      
      -- Efeito de gelo (cristais)
      temGelo = any (\p -> tipoProjetil p == Gelo) projeteis
      efeitoGelo = if temGelo
        then Color (withAlpha 0.65 (makeColorI 111 150 168 255)) $
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
      corVida = if percentualVida > 0.5 then makeColorI 135 174 111 255
                else if percentualVida > 0.25 then makeColorI 226 194 95 255
                else makeColorI 190 82 72 255
   in Translate posX posY $ Pictures [
        Color corTextoSuave $ rectangleWire (larguraTotal + 2) 6,
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
        Projetil Fogo _ -> makeColorI 175 88 58 255
        Projetil Gelo _ -> makeColorI 111 150 168 255
        Projetil Resina _ -> makeColorI 145 107 61 255
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
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y)
   in Translate posX posY $ modeloPortal bloco

modeloPortal :: Float -> Picture
modeloPortal bloco =
  Pictures [
    Color (makeColorI 38 34 50 255) $ ThickCircle (bloco * 0.22) (bloco * 0.22),
    Color (withAlpha 0.75 (makeColorI 91 76 130 255)) $ circleSolid (bloco * 0.25),
    Color (withAlpha 0.55 (makeColorI 170 132 210 255)) $ circleSolid (bloco * 0.12)
  ]

-- ============================================================================
-- DESENHO DA BASE
-- ============================================================================

desenhaBase :: ImmutableTowers -> Base -> Picture
desenhaBase e base =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoBase base
      posX = converteX (realToFrac x)
      posY = converteY (realToFrac y)
   in Translate posX posY $ modeloBase bloco

modeloBase :: Float -> Picture
modeloBase bloco =
  let pedra = makeColorI 110 113 105 255
      luz = makeColorI 173 177 166 255
      telhado = makeColorI 83 72 61 255
   in Pictures [
        Color (withAlpha 0.35 black) $ Translate 2 (-3) $ rectangleSolid (bloco * 0.8) (bloco * 0.72),
        Color pedra $ rectangleSolid (bloco * 0.72) (bloco * 0.62),
        Color luz $ Translate 0 (bloco * 0.18) $ rectangleSolid (bloco * 0.58) (bloco * 0.12),
        Color telhado $ Translate 0 (bloco * 0.36) $ Polygon [(-bloco*0.44,0),(0,bloco*0.22),(bloco*0.44,0)],
        Color (withAlpha 0.28 black) $ rectangleWire (bloco * 0.74) (bloco * 0.64)
      ]

-- ============================================================================
-- HUD (HEADS-UP DISPLAY)
-- ============================================================================

desenhaHUD :: ImmutableTowers -> Picture
desenhaHUD e =
  let base = baseJogo (jogo e)
      vida = floor (vidaBase base) :: Int
      creditos = creditosBase base
      totalOndas = sum $ map (length . inimigosOnda) $ concatMap ondasPortal $ portaisJogo (jogo e)
      inimigosAtivos = length (inimigosJogo (jogo e))
      y = alturaJanela/2 - 46
      painel = Color corPainel $ Translate 0 y $ rectangleSolid (larguraJanela - 96) 72
      texto x label valor corValor = Translate x (y - 8) $ Pictures [
        Color corTextoSuave $ Scale 0.13 0.13 $ Text label,
        Translate 110 0 $ Color corValor $ Scale 0.17 0.17 $ Text valor
        ]
      vidaCor = if vida > 40 then makeColorI 135 174 111 255 else makeColorI 190 82 72 255
   in Pictures [
        painel,
        Color (makeColorI 65 75 61 255) $ Translate 0 y $ rectangleWire (larguraJanela - 96) 72,
        texto (-larguraJanela/2 + 74) "VIDA" (show vida) vidaCor,
        texto (-80) "CREDITOS" (show creditos) (makeColorI 226 194 95 255),
        texto (larguraJanela/2 - 330) "INIMIGOS" (show inimigosAtivos ++ " / " ++ show totalOndas) (makeColorI 226 153 76 255)
      ]

desenhaPainelLateral :: ImmutableTowers -> Picture
desenhaPainelLateral e =
  let base = baseJogo (jogo e)
      torreSel = torreSelecionada e
      inimigosAtivos = length (inimigosJogo (jogo e))
      ondasRestantes = length $ concatMap ondasPortal (portaisJogo (jogo e))
      x = larguraJanela/2 - 150
      y = 120
      linha n texto = Translate (x - 110) (y + 86 - fromIntegral n * 34) $
        Scale 0.085 0.085 $ Color corTextoSuave $ Text texto
      titulo = Translate (x - 110) (y + 122) $
        Scale 0.12 0.12 $ Color (makeColorI 226 194 95 255) $ Text "ESTADO"
      infoTorre = case torreSel of
        Nothing -> ["Seleciona uma torre", "na loja para ver", "a pre-visualizacao."]
        Just torre -> [
          "Torre: " ++ nomeProjetil (projetilTorre torre),
          "Alcance: " ++ show (floor $ alcanceTorre torre :: Int),
          "Dano: " ++ show (floor $ danoTorre torre :: Int),
          "Direito: cancelar"
          ]
      linhas = [
        "Vida base: " ++ show (floor (vidaBase base) :: Int),
        "Creditos: " ++ show (creditosBase base),
        "Inimigos: " ++ show inimigosAtivos,
        "Ondas: " ++ show ondasRestantes
        ] ++ [""] ++ infoTorre
   in Pictures [
        Color corPainel $ Translate x y $ rectangleSolid 260 330,
        Color (makeColorI 68 78 63 255) $ Translate x y $ rectangleWire 260 330,
        titulo,
        Pictures [linha i texto | (i, texto) <- zip [0 :: Int ..] linhas]
      ]

-- ============================================================================
-- LOJA (SHOP) - ATUALIZADA
-- ============================================================================

desenhaLoja :: ImmutableTowers -> Picture
desenhaLoja e =
  let loja = lojaJogo (jogo e)
      bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      torreSel = torreSelecionada e
      creditos = creditosBase (baseJogo (jogo e))
   in Pictures $ zipWith (desenhaBotaoLoja bloco torreSel creditos) [0..] loja

desenhaBotaoLoja :: Float -> Maybe Torre -> Creditos -> Int -> (Creditos, Torre) -> Picture
desenhaBotaoLoja _ torreSel creditos indice (preco, torre) =
  let posX = -larguraJanela/2 + 118 + fromIntegral indice * 136
      posY = -alturaJanela/2 + 82
      compravel = creditos >= preco
      selecionada = case torreSel of
                      Just t -> tipoProjetil (projetilTorre t) == 
                                tipoProjetil (projetilTorre torre)
                      Nothing -> False
      cor = if selecionada then makeColorI 226 194 95 255 else if compravel then makeColorI 92 103 88 255 else makeColorI 78 70 66 255
      corFundo = if selecionada 
                 then makeColorI 55 60 45 235
                 else if compravel then makeColorI 31 39 33 225 else makeColorI 28 28 27 215
      corPreco = if compravel then makeColorI 226 194 95 255 else makeColorI 190 82 72 255
      textoInfo = if selecionada 
                  then Pictures [
                    Translate (-44) 39 $ Scale 0.065 0.065 $ Color corTextoSuave $ Text ("ALC " ++ show (floor $ alcanceTorre torre :: Int)),
                    Translate (-44) 27 $ Scale 0.065 0.065 $ Color corTextoSuave $ Text ("DMG " ++ show (floor $ danoTorre torre :: Int))
                  ]
                  else Blank
   in Translate posX posY $ Pictures [
        Color corFundo $ rectangleSolid 116 116,
        Color cor $ rectangleWire 116 116,
        if selecionada then Color cor $ rectangleWire 120 120 else Blank,
        Translate 0 4 $ modeloTorre 56 (projetilTorre torre) selecionada,
        if compravel then Blank else Color (withAlpha 0.45 black) $ rectangleSolid 116 116,
        Translate (-45) (-48) $ Scale 0.075 0.075 $ Color corTextoSuave $ Text (nomeProjetil (projetilTorre torre)),
        Translate 18 (-48) $ Scale 0.085 0.085 $ Color corPreco $ Text (show preco),
        textoInfo
      ]

nomeProjetil :: Projetil -> String
nomeProjetil projetil = case tipoProjetil projetil of
  Resina -> "RESINA"
  Gelo -> "GELO"
  Fogo -> "FOGO"

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
  desenhaOverlayEstado "VITORIA" (makeColorI 135 174 111 255) "A base resistiu a todas as ondas." "ESC para sair"

desenhaDerrota :: Picture
desenhaDerrota =
  desenhaOverlayEstado "DERROTA" (makeColorI 190 82 72 255) "A base foi destruida." "ESC para sair"

desenhaPausado :: Picture
desenhaPausado =
  desenhaOverlayEstado "PAUSADO" (makeColorI 226 194 95 255) "O combate esta parado." "P para continuar"

desenhaOverlayEstado :: String -> Color -> String -> String -> Picture
desenhaOverlayEstado titulo corTitulo corpo ajuda =
  Pictures [
    Color (withAlpha 0.82 black) $ rectangleSolid larguraJanela alturaJanela,
    Color corPainel $ rectangleSolid 560 260,
    Color (makeColorI 68 78 63 255) $ rectangleWire 560 260,
    Translate (-180) 64 $ Scale 0.34 0.34 $ Color corTitulo $ Text titulo,
    Translate (-190) (-18) $ Scale 0.13 0.13 $ Color corTextoSuave $ Text corpo,
    Translate (-150) (-86) $ Scale 0.1 0.1 $ Color (makeColorI 154 164 146 255) $ Text ajuda
  ]

-- ============================================================================
-- FUNÇÕES AUXILIARES
-- ============================================================================

blocoToPicture :: Terreno -> Int -> Int -> Float -> Picture
blocoToPicture terreno x y bloco =
  let variacao = (x * 17 + y * 31) `mod` 5
      relva = if even (x + y) then corRelvaBase else corRelvaAlt
      detalheRelva = if variacao == 0
                     then Color (withAlpha 0.18 (makeColorI 152 168 121 255)) $
                          rectangleSolid (bloco * 0.34) (bloco * 0.34)
                     else Blank
      grelha = Color (withAlpha 0.16 black) $ rectangleWire bloco bloco
   in case terreno of
        Relva -> Pictures [
          Color relva $ rectangleSolid bloco bloco,
          detalheRelva,
          grelha
          ]
        Terra -> Pictures [
          Color corTerraBase $ rectangleSolid bloco bloco,
          Color (withAlpha 0.2 black) $ rectangleWire bloco bloco,
          Color (withAlpha 0.12 (makeColorI 142 111 74 255)) $ rectangleSolid (bloco * 0.72) (bloco * 0.72)
          ]
        Agua -> Pictures [
          Color corAguaBase $ rectangleSolid bloco bloco,
          Color (withAlpha 0.22 (makeColorI 141 177 188 255)) $ rectangleSolid (bloco * 0.72) (bloco * 0.72),
          Color (withAlpha 0.18 black) $ rectangleWire bloco bloco
          ]

mapaToPicture :: Imagens -> Mapa -> Picture
mapaToPicture _ terreno =
  let bloco = calculaTamanhoBloco terreno
      largura = fromIntegral (length (head terreno))
      altura = fromIntegral (length terreno)
      offsetX = offsetMapaX terreno
      offsetY = offsetMapaY terreno
      margem = 14
   in Pictures [
        Color (makeColorI 20 25 21 255) $
          Translate 0 mapaCentroY $ rectangleSolid (largura * bloco + margem) (altura * bloco + margem),
        Color (makeColorI 68 78 63 255) $
          Translate 0 mapaCentroY $ rectangleWire (largura * bloco + margem) (altura * bloco + margem),
        Pictures [
          Translate
            (offsetX + fromIntegral x * bloco + bloco / 2)
            (offsetY + fromIntegral y * bloco + bloco / 2)
            $ blocoToPicture b x y bloco
          | (y, linha) <- zip [0 :: Int ..] (reverse terreno),
            (x, b) <- zip [0 :: Int ..] linha
        ]
      ]

calculaTamanhoBloco :: Mapa -> Float
calculaTamanhoBloco terreno =
  let largura = fromIntegral (length (head terreno))
      altura = fromIntegral (length terreno)
   in min (mapaLarguraMax / largura) (mapaAlturaMax / altura)

converteX :: Double -> Float
converteX x = offsetMapaX mapa01 + (double2Float x * calculaTamanhoBloco mapa01)

converteY :: Double -> Float
converteY y = offsetMapaY mapa01 + (fromIntegral (length mapa01) - double2Float y) * calculaTamanhoBloco mapa01

offsetMapaX :: Mapa -> Float
offsetMapaX terreno =
  let bloco = calculaTamanhoBloco terreno
      largura = fromIntegral (length (head terreno))
   in -(largura * bloco) / 2

offsetMapaY :: Mapa -> Float
offsetMapaY terreno =
  let bloco = calculaTamanhoBloco terreno
      altura = fromIntegral (length terreno)
   in mapaCentroY - (altura * bloco) / 2

escalaImagem :: Float -> Float -> Float
escalaImagem bloco tamanhoOriginal = bloco / tamanhoOriginal

carregarImagens :: IO ImmutableTowers
carregarImagens = do
  fundo <- loadJuicy (caminhoImagem "fundo_1_.bmp")
  grass <- loadJuicy (caminhoImagem "images.bmp")
  water <- loadJuicy (caminhoImagem "6da00a37f26551f688dcc04367d7c73c_1.bmp")
  land <- loadJuicy (caminhoImagem "terra_textura.bmp")
  torreResina <- loadJuicy (caminhoImagem "DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp")
  torreGelo <- loadJuicy (caminhoImagem "DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp")
  torreFogo <- loadJuicy (caminhoImagem "DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp")
  inimigo <- loadJuicy (caminhoImagem "enemy-clipart-little-monster-holding-a-gun-in-one-hand_546721_wh860_2_-removebg-preview.bmp")
  base <- loadJuicy (caminhoImagem "tower_image-removebg-preview.bmp")
  portal <- loadJuicy (caminhoImagem "portal.bmp")

  -- Estes botões eram carregados a partir de ficheiros que não existem no
  -- repositório. Por agora são desenhados com texto em 'desenhaMenu'.
  let playImg = Nothing
      exitImg = Nothing
      creditosImg = Nothing
      tutorialImg = Nothing

  let imgs = [
        (Fundo, fundo), (Grass, grass), (Water, water), (Land, land),
        (TorreResina, torreResina), (TorreGelo, torreGelo), (TorreFogo, torreFogo),
        (Inimigocima, inimigo), (BaseFoto, base), (PortalFoto, portal),
        (Play, playImg), (Exit, exitImg), (ButaoCreditos, creditosImg),
        (ImagemTutorial, tutorialImg), (ImagemCreditos, creditosImg)
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
  
  return $ ImmutableTowers jogoInicial imgs (MenuInicial Jogar) 0 Nothing Nothing

ratoParaCelula :: Mapa -> (Float, Float) -> Maybe (Int, Int, Float, Float)
ratoParaCelula mapa (mx, my) =
  let bloco = calculaTamanhoBloco mapa
      altura = fromIntegral (length mapa)
      offsetX = offsetMapaX mapa
      offsetY = offsetMapaY mapa
      cx = floor ((mx - offsetX) / bloco) :: Int
      cy = floor (altura - ((my - offsetY) / bloco)) :: Int
      dentro = cx >= 0 && cy >= 0 && cx < length (head mapa) && cy < length mapa
      centroX = offsetX + (fromIntegral cx + 0.5) * bloco
      centroY = offsetY + (altura - fromIntegral cy - 0.5) * bloco
   in if dentro then Just (cx, cy, centroX, centroY) else Nothing

terrenoEm :: Int -> Int -> Mapa -> Maybe Terreno
terrenoEm x y mapa
  | y < 0 || y >= length mapa = Nothing
  | x < 0 || x >= length (head mapa) = Nothing
  | otherwise = Just ((mapa !! y) !! x)

caminhoImagem :: FilePath -> FilePath
caminhoImagem nome = "app/imagens/" ++ nome