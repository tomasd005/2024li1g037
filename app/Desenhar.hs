module Desenhar where

import qualified BotSystem as Bot
import Data.Maybe (fromMaybe)
import GameFactory
import Graphics.Gloss
import Graphics.Gloss.Juicy
import ImmutableTowers
import LI12425
import MapData
import MapGeometry
import SaveSystem
import Tarefa2 (inimigosNoAlcance)
import TowerSystem
import UIComponents

-- Paleta mais sóbria, inspirada em tower defenses com tabuleiro tático:
-- terreno escuro, água dessaturada, UI em painéis translúcidos.
mapaLarguraMax, mapaAlturaMax, mapaCentroY :: Float
mapaLarguraMax = 900
mapaAlturaMax = 840
mapaCentroY = 8

renderLayout :: MapLayoutConfig
renderLayout = MapLayoutConfig mapaLarguraMax mapaAlturaMax mapaCentroY

corFundoJogo, corPainel, corTextoSuave :: Color
corFundoJogo = makeColorI 34 50 34 255
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
desenha e = case modo e of
  MenuInicial opcao -> return $ desenhaMenu e opcao
  Pausado -> return $ desenhaPausado e
  EmJogo -> return $ desenhaJogoCompleto e
  TutorialFoto -> return desenhaTutorial
  MostrarCreditos -> return desenhaCreditos
  MostrarPerfil -> return $ desenhaPerfil e
  MostrarLeaderboard -> return $ desenhaLeaderboard e
  MostrarOpcoes -> return $ desenhaOpcoes e
  SelecionarModo -> return $ desenhaSelecionarModo e
  EditorMapa -> return $ desenhaEditorMapa e

-- ============================================================================
-- DESENHO DO JOGO EM MODO EMJOGO - ATUALIZADO
-- ============================================================================

desenhaJogoCompleto :: ImmutableTowers -> Picture
desenhaJogoCompleto e =
  Pictures [
    Color corFundoJogo $ rectangleSolid larguraJanela alturaJanela,
    Color (makeColorI 22 29 24 235) $ Translate (larguraJanela/2 - 118) 0 $ rectangleSolid 236 alturaJanela,
    Color (makeColorI 23 30 25 240) $ Translate 0 (-alturaJanela/2 + 70) $ rectangleSolid larguraJanela 140,
    Color (makeColorI 23 30 25 240) $ Translate 0 (alturaJanela/2 - 40) $ rectangleSolid larguraJanela 80,
    -- Camadas de baixo para cima
    mapaToPicture (imagens e) (mapaJogo (jogo e)),
    desenhaPreVisualizacaoColocacao e,
    desenhaTorres e,
    desenhaInimigosComEfeitos e,
    desenhaProjeteis e,
    desenhaPortais e,
    desenhaBase e (baseJogo (jogo e)),
    desenhaHUD e,
    desenhaControlesJogo e,
    desenhaPainelLateral e,
    desenhaLoja e,
    desenhaMensagens e,
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
      Translate (-larguraJanela/2 + 38) (-alturaJanela/2 + 132) $
      Scale 0.08 0.08 $ Color corTextoSuave $ 
      Text "Loja: clique para comprar | X alterna 1x/2x"
    Just _ ->
      Translate (-larguraJanela/2 + 38) (-alturaJanela/2 + 132) $
      Scale 0.08 0.08 $ Color (makeColorI 235 194 96 255) $ 
      Text "Clique na RELVA para colocar (Botao direito cancela)"

-- ============================================================================
-- DESENHO DO MENU
-- ============================================================================

desenhaMenu :: ImmutableTowers -> MenuInicialOpcoes -> Picture
desenhaMenu e opcaoSel =
  Pictures [
    Color corFundoJogo $ rectangleSolid larguraJanela alturaJanela,
    Color (withAlpha 0.35 (makeColorI 40 52 43 255)) $ Translate 0 70 $ rectangleSolid 920 620,
    Color (makeColorI 68 78 63 255) $ Translate 0 70 $ rectangleWire 920 620,
    Translate (-300) 300 $ Scale 0.42 0.42 $ Color corTextoSuave $ Text "IMMUTABLE",
    Translate (-228) 240 $ Scale 0.42 0.42 $ Color (makeColorI 226 194 95 255) $ Text "TOWERS",
    Translate (-300) 180 $ Scale 0.105 0.105 $ Color (makeColorI 154 164 146 255) $
      Text "Defende a base, evolui torres e compete por pontuacao.",
    Translate (-300) 145 $ Scale 0.095 0.095 $ Color corTextoSuave $
      Text ("Perfil: " ++ nomeJogador (perfilJogador e) ++ "   Modo: " ++ nomeModoJogo (modoJogoEscolhido e)),
    botao (-280) 55 opcaoSel Jogar "JOGAR" "Comecar defesa",
    botao 0 55 opcaoSel Modos "MODOS" "Modo/dificuldade",
    botao 280 55 opcaoSel Perfil "PERFIL" "Conta local",
    botao (-280) (-105) opcaoSel Leaderboard "RANKING" "Melhores scores",
    botao 0 (-105) opcaoSel Creditos "AJUDA" "Como jogar",
    botao 280 (-105) opcaoSel Opcoes "OPCOES" "Interface e controlos",
    botao 0 (-255) opcaoSel Sair "SAIR" "Fechar jogo",
    Translate (-300) (-365) $ Scale 0.105 0.105 $ Color (makeColorI 154 164 146 255) $
      Text "Setas/Enter ou clique nos botoes | E abre editor"
  ]
  where
    botao x y sel alvo titulo subtitulo =
      let selecionado = sel == alvo
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
    "5. P pausa, U melhora torre, S/L guarda/carrega, B chama bot, O cria obstaculo."
  ]

desenhaCreditos :: Picture
desenhaCreditos = desenhaPainelTexto "CREDITOS" [
    "Immutable Towers",
    "Projeto LI1 — Tower Defense em Haskell + Gloss.",
    "UI, feedback de colocacao e compatibilidade Windows/GHCup atualizados.",
    "Pressiona ENTER ou ESC para voltar."
  ]

desenhaPerfil :: ImmutableTowers -> Picture
desenhaPerfil e =
  let perfil = perfilJogador e
   in desenhaPainelTexto "PERFIL" [
        "Nome: " ++ nomeJogador perfil,
        "Jogos: " ++ show (jogosJogador perfil),
        "Vitorias: " ++ show (vitoriasJogador perfil),
        "Derrotas: " ++ show (derrotasJogador perfil),
        "Melhor score: " ++ show (melhorPontuacaoJogador perfil),
        "Escreve para alterar o nome. Backspace apaga."
      ]

desenhaLeaderboard :: ImmutableTowers -> Picture
desenhaLeaderboard e =
  let scores = take 8 (leaderboardLocal e)
      linhas = if null scores
        then ["Ainda nao ha pontuacoes.", "Completa partidas para preencher o ranking."]
        else zipWith linhaScore [1 :: Int ..] scores
   in desenhaPainelTexto "LEADERBOARD" linhas
  where
    linhaScore n score = show n ++ ". " ++ nomePontuacao score
      ++ " | " ++ nomeModoJogo (modoPontuacao score)
      ++ " | " ++ show (valorPontuacao score)
      ++ " pts | ondas " ++ show (ondasPontuacao score)

desenhaSelecionarModo :: ImmutableTowers -> Picture
desenhaSelecionarModo e =
  Pictures [
    Color corFundoJogo $ rectangleSolid larguraJanela alturaJanela,
    Translate (-430) 300 $ Scale 0.32 0.32 $ Color (makeColorI 226 194 95 255) $ Text "MODOS",
    Translate (-430) 258 $ Scale 0.095 0.095 $ Color corTextoSuave $
      Text ("Atual: " ++ nomeModoJogo (modoJogoEscolhido e) ++ " | Clique num modo ou usa esquerda/direita"),
    modoCard e ModoHistoria (-360) 90 "HISTORIA" "4 ondas, progressao normal" "150 creditos / 80 vida",
    modoCard e ModoInfinito 0 90 "INFINITO" "ondas geradas sem fim" "165 creditos / score alto",
    modoCard e ModoDesafio 360 90 "DESAFIO" "ondas fortes mais cedo" "115 creditos / 55 vida",
    modoCard e ModoBoss (-180) (-105) "BOSS" "poucos inimigos muito duros" "180 creditos / 100 vida",
    modoCard e ModoSandbox 180 (-105) "SANDBOX" "testar builds e upgrades" "999 creditos / loja barata",
    Translate (-150) (-310) $ Scale 0.1 0.1 $ Color (makeColorI 154 164 146 255) $ Text "ENTER volta ao menu | JOGAR usa o modo selecionado"
  ]

desenhaOpcoes :: ImmutableTowers -> Picture
desenhaOpcoes e =
  let rato = posicaoRato e
   in Pictures [
        Color corFundoJogo $ rectangleSolid larguraJanela alturaJanela,
        drawPanel (UIRect 0 40 820 590),
        Translate (-335) 270 $ Scale 0.3 0.3 $ Color (makeColorI 226 194 95 255) $ Text "OPCOES",
        Translate (-335) 214 $ Scale 0.095 0.095 $ Color corTextoSuave $ Text "Interface otimizada para janela 1152x1080",
        Translate (-335) 156 $ Scale 0.095 0.095 $ Color corTextoSuave $ Text "Controlos principais:",
        Translate (-335) 102 $ Scale 0.083 0.083 $ Color (makeColorI 190 201 180 255) $ Text "Mouse: comprar, construir, selecionar, melhorar e vender torres",
        Translate (-335) 58 $ Scale 0.083 0.083 $ Color (makeColorI 190 201 180 255) $ Text "P pausa | X alterna 1x/2x/4x | S/L guarda/carrega | B sugestao bot",
        Translate (-335) 4 $ Scale 0.083 0.083 $ Color (makeColorI 190 201 180 255) $ Text "E abre editor de mapa no menu | ESC volta aos menus",
        Translate (-335) (-76) $ Scale 0.095 0.095 $ Color corTextoSuave $ Text "Distribuicao:",
        Translate (-335) (-126) $ Scale 0.078 0.078 $ Color (makeColorI 190 201 180 255) $ Text "Assets atuais em app/imagens. Para release: copiar exe + pasta app/imagens.",
        Translate (-335) (-166) $ Scale 0.078 0.078 $ Color (makeColorI 190 201 180 255) $ Text "Windows: cabal build -O2 e empacotar dist-newstyle exe com DLLs necessarias.",
        drawButton rato (UIRect 0 (-245) 180 48) Neutral "ENTER / ESC"
      ]

modoCard :: ImmutableTowers -> ModoJogoEscolhido -> Float -> Float -> String -> String -> String -> Picture
modoCard e modoCardAtual x y titulo desc regras =
  let selecionado = modoJogoEscolhido e == modoCardAtual
      fundo = if selecionado then makeColorI 55 60 45 245 else makeColorI 25 34 29 230
      borda = if selecionado then makeColorI 226 194 95 255 else makeColorI 85 102 82 255
   in Translate x y $ Pictures [
        Color fundo $ rectangleSolid 300 152,
        Color borda $ rectangleWire 300 152,
        if selecionado then Color (withAlpha 0.35 borda) $ Translate 0 70 $ rectangleSolid 270 6 else Blank,
        Translate (-120) 34 $ Scale 0.15 0.15 $ Color corTextoSuave $ Text titulo,
        Translate (-120) (-8) $ Scale 0.071 0.071 $ Color (makeColorI 190 201 180 255) $ Text desc,
        Translate (-120) (-42) $ Scale 0.069 0.069 $ Color (makeColorI 226 194 95 255) $ Text regras
      ]

desenhaEditorMapa :: ImmutableTowers -> Picture
desenhaEditorMapa e =
  Pictures [
    Color corFundoJogo $ rectangleSolid larguraJanela alturaJanela,
    mapaToPicture (imagens e) (mapaJogo (jogo e)),
    Translate (-larguraJanela/2 + 44) (alturaJanela/2 - 54) $
      Scale 0.13 0.13 $ Color (makeColorI 226 194 95 255) $ Text "EDITOR DE MAPA",
    Translate (-larguraJanela/2 + 44) (alturaJanela/2 - 86) $
      Scale 0.09 0.09 $ Color corTextoSuave $ Text "Clique numa celula: relva -> terra -> asfalto -> agua. ENTER/ESC volta."
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
      foco = if torreFocada e == Just (posicaoTorre torre)
             then desenhaFocoTorre e torre
             else Blank
   in [alcance, foco, sprite, cooldown]

desenhaFocoTorre :: ImmutableTowers -> Torre -> Picture
desenhaFocoTorre e torre =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (x, y) = posicaoTorre torre
   in Color (makeColorI 226 194 95 255) $
        Translate (converteX (realToFrac x)) (converteY (realToFrac y)) $
        ThickCircle (bloco * 0.34) 3

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
  let corpo = makeColorI 66 72 69 255
      sombra = makeColorI 14 18 16 170
      metalClaro = makeColorI 186 192 184 255
      metalEscuro = makeColorI 38 43 41 255
      tipo = tipoProjetil projetil
      acento = case tipo of
        Resina -> makeColorI 145 107 61 255
        Gelo -> makeColorI 111 150 168 255
        Fogo -> makeColorI 175 88 58 255
        Medo -> makeColorI 170 132 210 255
        Veneno -> makeColorI 101 168 92 255
        Eletrico -> makeColorI 226 194 95 255
      escala = bloco / 28
      aro = if selecionada
            then Color (makeColorI 226 194 95 255) $ rectangleWire (bloco * 0.92) (bloco * 0.92)
            else Blank
      topo = case tipo of
        Resina -> Pictures [Color acento $ Translate 0 12 $ ThickCircle 4 4, Color metalClaro $ Translate 0 12 $ circleSolid 2]
        Gelo -> Pictures [
          Color acento $ Translate 0 12 $ Polygon [(-7,-3),(0,8),(7,-3),(0,-8)],
          Color metalClaro $ Translate 0 12 $ rectangleSolid 2 13
          ]
        Fogo -> Pictures [
          Color acento $ Translate 0 12 $ Polygon [(-7,-5),(0,9),(7,-5)],
          Color (makeColorI 226 194 95 255) $ Translate 0 12 $ Polygon [(-3,-2),(0,5),(3,-2)]
          ]
        Medo -> Pictures [Color acento $ Translate 0 12 $ ThickCircle 6 3, Color acento $ Translate 0 12 $ rectangleSolid 12 2]
        Veneno -> Pictures [Color acento $ Translate 0 12 $ circleSolid 6, Color (withAlpha 0.55 metalClaro) $ Translate (-2) 14 $ circleSolid 2]
        Eletrico -> Color acento $ Translate 0 12 $ ThickCircle 5 3
   in Pictures [
        Color sombra $ Translate 2 (-3) $ rectangleSolid (bloco * 0.62) (bloco * 0.72),
        aro,
        Scale escala escala $ Pictures [
          Color metalEscuro $ Polygon [(-11,-10),(11,-10),(8,8),(-8,8)],
          Color corpo $ Polygon [(-8,-8),(8,-8),(6,8),(-6,8)],
          Color acento $ Translate 0 (-1) $ rectangleSolid 5 15,
          Color metalClaro $ Translate 0 8 $ rectangleSolid 18 4,
          Color metalClaro $ Translate 0 (-10) $ rectangleSolid 21 5,
          Color (withAlpha 0.38 black) $ Translate 5 0 $ Polygon [(0,-8),(3,-8),(2,8),(-1,8)],
          topo
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
      temMedo = any (\p -> tipoProjetil p == Medo) projeteis
      efeitoMedo = if temMedo
        then Color (withAlpha 0.8 (makeColorI 170 132 210 255)) $
               Translate posX (posY + bloco * 0.38) $ Scale 0.12 0.12 $ Text "!"
        else Blank
      temVeneno = any (\p -> tipoProjetil p == Veneno) projeteis
      efeitoVeneno = if temVeneno
        then Color (withAlpha 0.55 (makeColorI 101 168 92 255)) $
               Translate posX posY $ ThickCircle (bloco/4) 3
        else Blank
      temEletrico = any (\p -> tipoProjetil p == Eletrico) projeteis
      efeitoEletrico = if temEletrico
        then Color (withAlpha 0.8 (makeColorI 226 194 95 255)) $
               Translate posX posY $ Line [(-bloco/4, -bloco/4), (0, bloco/4), (bloco/5, 0), (bloco/3, bloco/3)]
        else Blank
   
   in Pictures [efeitoFogo, efeitoGelo, efeitoResina, efeitoMedo, efeitoVeneno, efeitoEletrico]

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
        Projetil Medo _ -> makeColorI 170 132 210 255
        Projetil Veneno _ -> makeColorI 101 168 92 255
        Projetil Eletrico _ -> makeColorI 226 194 95 255
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
      speed = show (round (velocidadeJogo e) :: Int) ++ "X"
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
        texto (larguraJanela/2 - 378) "INIMIGOS" (show inimigosAtivos ++ " / " ++ show totalOndas) (makeColorI 226 153 76 255),
        texto (larguraJanela/2 - 178) "SPEED" speed (makeColorI 111 150 168 255)
      ]

desenhaControlesJogo :: ImmutableTowers -> Picture
desenhaControlesJogo e =
  let rato = posicaoRato e
      speed = velocidadeJogo e
      toneSpeed alvo = if abs (speed - alvo) < 0.1 then Primary else Neutral
   in Pictures [
        drawButton rato startWaveRect Primary "VAGA",
        drawButton rato pauseRect Neutral "PAUSA",
        drawButton rato speed1Rect (toneSpeed 1) "1X",
        drawButton rato speed2Rect (toneSpeed 2) "2X",
        drawButton rato speed4Rect (toneSpeed 4) "4X"
      ]

desenhaPainelLateral :: ImmutableTowers -> Picture
desenhaPainelLateral e =
  let base = baseJogo (jogo e)
      torreSel = torreSelecionada e
      torreAtiva = torreFocada e >>= \pos -> findTorreNaPosicao pos (torresJogo (jogo e))
      inimigosAtivos = length (inimigosJogo (jogo e))
      ondasRestantes = length $ concatMap ondasPortal (portaisJogo (jogo e))
      x = larguraJanela/2 - 118
      y = 118
      linha n texto = Translate (x - 92) (y + 86 - fromIntegral n * 28) $
        Scale 0.071 0.071 $ Color corTextoSuave $ Text texto
      titulo = Translate (x - 92) (y + 118) $
        Scale 0.12 0.12 $ Color (makeColorI 226 194 95 255) $ Text "ESTADO"
      infoTorre = case torreAtiva of
        Just torre -> [
          "Torre: " ++ nomeProjetil (projetilTorre torre),
          "Dano: " ++ show (floor $ danoTorre torre :: Int),
          "Alcance: " ++ show (floor $ alcanceTorre torre :: Int),
          "Rajada: " ++ show (rajadaTorre torre),
          "Upgrade: " ++ show (custoUpgradeTorre torre) ++ " cred",
          "Tecla U: melhorar"
          ]
        Nothing -> case torreSel of
          Nothing -> ["Seleciona loja", "ou uma torre", "no mapa."]
          Just torre -> [
            "Comprar: " ++ nomeProjetil (projetilTorre torre),
            "Alcance: " ++ show (floor $ alcanceTorre torre :: Int),
            "Dano: " ++ show (floor $ danoTorre torre :: Int),
            "Direito: cancelar"
            ]
      linhas = [
        "Vida base: " ++ show (floor (vidaBase base) :: Int),
        "Creditos: " ++ show (creditosBase base),
        "Modo: " ++ nomeModoJogo (modoJogoEscolhido e),
        "Inimigos: " ++ show inimigosAtivos,
        "Ondas: " ++ show ondasRestantes,
        "Bot: " ++ Bot.sugestaoBot (jogo e),
        "Velocidade: " ++ if velocidadeJogo e >= 2 then "2x" else "1x"
        ] ++ [""] ++ infoTorre
   in Pictures [
        Color (withAlpha 0.84 corPainel) $ Translate x y $ rectangleSolid 218 330,
        Color (makeColorI 68 78 63 255) $ Translate x y $ rectangleWire 218 330,
        titulo,
        Pictures [linha i texto | (i, texto) <- zip [0 :: Int ..] linhas],
        drawButton (posicaoRato e) upgradeRect (maybe Disabled (const Primary) torreAtiva) "UPGRADE",
        drawButton (posicaoRato e) sellRect (maybe Disabled (const Danger) torreAtiva) "VENDER",
        drawButton (posicaoRato e) cancelRect Neutral "CANCELAR"
      ]

findTorreNaPosicao :: Posicao -> [Torre] -> Maybe Torre
findTorreNaPosicao _ [] = Nothing
findTorreNaPosicao pos (torre:torres)
  | posicaoTorre torre == pos = Just torre
  | otherwise = findTorreNaPosicao pos torres

-- ============================================================================
-- LOJA (SHOP) - ATUALIZADA
-- ============================================================================

desenhaLoja :: ImmutableTowers -> Picture
desenhaLoja e =
  let loja = lojaJogo (jogo e)
      bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      torreSel = torreSelecionada e
      creditos = creditosBase (baseJogo (jogo e))
   in Pictures $
        [ Color (withAlpha 0.86 corPainel) $ Translate (-170) (-alturaJanela/2 + 69) $ rectangleSolid 790 116,
          Color (makeColorI 68 78 63 255) $ Translate (-170) (-alturaJanela/2 + 69) $ rectangleWire 790 116
        ] ++ zipWith (desenhaBotaoLoja bloco torreSel creditos) [0..] loja

desenhaBotaoLoja :: Float -> Maybe Torre -> Creditos -> Int -> (Creditos, Torre) -> Picture
desenhaBotaoLoja _ torreSel creditos indice (preco, torre) =
  let posX = -larguraJanela/2 + 74 + fromIntegral indice * 82
      posY = -alturaJanela/2 + 72
      compravel = creditos >= preco
      selecionada = case torreSel of
                      Just t -> tipoProjetil (projetilTorre t) == tipoProjetil (projetilTorre torre)
                        && danoTorre t == danoTorre torre
                      Nothing -> False
      cor = if selecionada then makeColorI 226 194 95 255 else if compravel then makeColorI 92 103 88 255 else makeColorI 78 70 66 255
      corFundo = if selecionada 
                 then makeColorI 55 60 45 235
                 else if compravel then makeColorI 31 39 33 225 else makeColorI 28 28 27 215
      corPreco = if compravel then makeColorI 226 194 95 255 else makeColorI 190 82 72 255
      textoInfo = if selecionada 
                  then Pictures [
                    Translate (-31) 29 $ Scale 0.052 0.052 $ Color corTextoSuave $ Text ("A" ++ show (floor $ alcanceTorre torre :: Int)),
                    Translate (-31) 18 $ Scale 0.052 0.052 $ Color corTextoSuave $ Text ("D" ++ show (floor $ danoTorre torre :: Int))
                  ]
                  else Blank
   in Translate posX posY $ Pictures [
        Color corFundo $ rectangleSolid 72 104,
        Color cor $ rectangleWire 72 104,
        if selecionada then Color cor $ rectangleWire 76 108 else Blank,
        Translate 0 11 $ modeloTorre 44 (projetilTorre torre) selecionada,
        if compravel then Blank else Color (withAlpha 0.45 black) $ rectangleSolid 72 104,
        Translate (-28) (-36) $ Scale 0.048 0.048 $ Color corTextoSuave $ Text (nomeProjetilCurto (projetilTorre torre)),
        Translate 4 (-36) $ Scale 0.058 0.058 $ Color corPreco $ Text (show preco),
        textoInfo
      ]

desenhaMensagens :: ImmutableTowers -> Picture
desenhaMensagens e =
  Pictures [desenhaMensagem i msg | (i, msg) <- zip [0 :: Int ..] (mensagensUI e)]

desenhaMensagem :: Int -> MensagemUI -> Picture
desenhaMensagem indice msg =
  let x = -larguraJanela / 2 + 34
      y = alturaJanela / 2 - 106 - fromIntegral indice * 42
      cor = corMensagem (tipoMensagem msg)
      alpha = min 1 (max 0.35 (tempoMensagem msg / 2.4))
   in Translate x y $
        Pictures [
          Color (withAlpha (0.78 * alpha) (makeColorI 20 27 23 255)) $ Translate 132 0 $ rectangleSolid 264 34,
          Color (withAlpha alpha cor) $ Translate 2 0 $ rectangleSolid 4 30,
          Translate 16 (-8) $ Scale 0.073 0.073 $ Color (withAlpha alpha corTextoSuave) $ Text (textoMensagem msg)
        ]

corMensagem :: TipoMensagem -> Color
corMensagem tipo = case tipo of
  MsgInfo -> makeColorI 111 150 168 255
  MsgSucesso -> makeColorI 135 174 111 255
  MsgAviso -> makeColorI 226 194 95 255
  MsgErro -> makeColorI 190 82 72 255

nomeProjetil :: Projetil -> String
nomeProjetil projetil = case tipoProjetil projetil of
  Resina -> "RESINA"
  Gelo -> "GELO"
  Fogo -> "FOGO"
  Medo -> "MEDO"
  Veneno -> "VENENO"
  Eletrico -> "ELETRICO"

nomeProjetilCurto :: Projetil -> String
nomeProjetilCurto projetil = case tipoProjetil projetil of
  Resina -> "RES"
  Gelo -> "GEL"
  Fogo -> "FOG"
  Medo -> "MED"
  Veneno -> "VEN"
  Eletrico -> "ELE"

nomeModoJogo :: ModoJogoEscolhido -> String
nomeModoJogo modoAtual = case modoAtual of
  ModoHistoria -> "HISTORIA"
  ModoInfinito -> "INFINITO"
  ModoDesafio -> "DESAFIO"
  ModoBoss -> "BOSS"
  ModoSandbox -> "SANDBOX"

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

desenhaPausado :: ImmutableTowers -> Picture
desenhaPausado e =
  Pictures [
    desenhaJogoCompleto (e {modo = EmJogo}),
    Color (withAlpha 0.78 black) $ rectangleSolid larguraJanela alturaJanela,
    drawPanel (UIRect 0 (-34) 420 340),
    Translate (-148) 88 $ Scale 0.31 0.31 $ Color (makeColorI 226 194 95 255) $ Text "PAUSA",
    Translate (-140) 42 $ Scale 0.09 0.09 $ Color corTextoSuave $ Text "O combate esta parado.",
    drawButton (posicaoRato e) resumeRect Primary "CONTINUAR",
    drawButton (posicaoRato e) restartRect Neutral "REINICIAR",
    drawButton (posicaoRato e) menuRect Danger "MENU"
  ]

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
        Asfalto -> Pictures [
          Color (makeColorI 48 52 50 255) $ rectangleSolid bloco bloco,
          Color (withAlpha 0.35 (makeColorI 205 190 120 255)) $ rectangleSolid (bloco * 0.68) (bloco * 0.08),
          Color (withAlpha 0.22 black) $ rectangleWire bloco bloco
          ]

mapaToPicture :: Imagens -> Mapa -> Picture
mapaToPicture _ terreno =
  let bloco = calculaTamanhoBloco terreno
      dims = fromMaybe (MapDimensions 0 0) (dimensoesMapa terreno)
      largura = fromIntegral (larguraMapa dims)
      altura = fromIntegral (alturaMapa dims)
      offsetX = offsetMapaX terreno
      offsetY = offsetMapaY terreno
      margem = 0
   in Pictures [
        Color (makeColorI 38 58 38 255) $
          Translate 0 mapaCentroY $ rectangleSolid (largura * bloco + margem) (altura * bloco + margem),
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
  fromMaybe 1 (tamanhoBloco renderLayout terreno)

converteX :: Double -> Float
converteX x = offsetMapaX mapa01 + (realToFrac x * calculaTamanhoBloco mapa01)

converteY :: Double -> Float
converteY y =
  let altura = maybe 0 (fromIntegral . alturaMapa) (dimensoesMapa mapa01)
   in offsetMapaY mapa01 + (altura - realToFrac y) * calculaTamanhoBloco mapa01

offsetMapaX :: Mapa -> Float
offsetMapaX terreno = maybe 0 fst (offsetMapa renderLayout terreno)

offsetMapaY :: Mapa -> Float
offsetMapaY terreno = maybe 0 snd (offsetMapa renderLayout terreno)

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

  (perfilGuardado, leaderboardGuardada, modoGuardado) <- carregarMetaEstado

  let imgs = [
        (Fundo, fundo), (Grass, grass), (Water, water), (Land, land),
        (TorreResina, torreResina), (TorreGelo, torreGelo), (TorreFogo, torreFogo),
        (Inimigocima, inimigo), (BaseFoto, base), (PortalFoto, portal),
        (Play, playImg), (Exit, exitImg), (ButaoCreditos, creditosImg),
        (ImagemTutorial, tutorialImg), (ImagemCreditos, creditosImg)
        ]
      
      jogoInicial = jogoParaModo modoGuardado
  
  return $ ImmutableTowers jogoInicial imgs (MenuInicial Jogar) 0 Nothing Nothing Nothing perfilGuardado leaderboardGuardada modoGuardado 0 False 1 []

ratoParaCelula :: Mapa -> (Float, Float) -> Maybe (Int, Int, Float, Float)
ratoParaCelula mapa (mx, my) =
  case ecraParaCelula renderLayout mapa (mx, my) of
    Just (cx, cy) -> do
      (centroX, centroY) <- mapaParaEcra renderLayout mapa (fromIntegral cx + 0.5, fromIntegral cy + 0.5)
      return (cx, cy, centroX, centroY)
    Nothing -> Nothing

terrenoEm :: Int -> Int -> Mapa -> Maybe Terreno
terrenoEm x y mapa = terrenoEmCelula mapa x y

caminhoImagem :: FilePath -> FilePath
caminhoImagem nome = "app/imagens/" ++ nome

startWaveRect, pauseRect, speed1Rect, speed2Rect, speed4Rect, upgradeRect, sellRect, cancelRect :: UIRect
startWaveRect = UIRect 178 500 126 42
pauseRect = UIRect 310 500 78 42
speed1Rect = UIRect 394 500 52 42
speed2Rect = UIRect 452 500 52 42
speed4Rect = UIRect 510 500 52 42
upgradeRect = UIRect 458 (-72) 92 42
sellRect = UIRect 458 (-122) 92 42
cancelRect = UIRect 458 (-172) 92 42

resumeRect, restartRect, menuRect :: UIRect
resumeRect = UIRect 0 (-28) 190 52
restartRect = UIRect 0 (-92) 190 52
menuRect = UIRect 0 (-156) 190 52
