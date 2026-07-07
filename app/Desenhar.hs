module Desenhar where

import Data.Maybe (fromMaybe)
import GameFactory
import Graphics.Gloss
import Graphics.Gloss.Juicy
import ImmutableTowers
import LI12425
import MapGeometry
import MetaTypes
import ProgressionSystem (capituloEstagioTexto)
import SaveSystem
import Tarefa2 (inimigosNoAlcance)
import TowerSystem
import UIComponents
import UIRects
import UIState

-- Paleta mais sóbria, inspirada em tower defenses com tabuleiro tático:
-- terreno escuro, água dessaturada, UI em painéis translúcidos.
mapaLarguraMax, mapaAlturaMax, mapaCentroY :: Float
mapaLarguraMax = 1280
mapaAlturaMax = 952
mapaCentroY = 18

renderLayout :: MapLayoutConfig
renderLayout = MapLayoutConfig mapaLarguraMax mapaAlturaMax mapaCentroY

corFundoJogo, corPainel, corTextoSuave :: Color
corFundoJogo = makeColorI 30 43 34 255
corPainel = makeColorI 21 27 24 232
corTextoSuave = makeColorI 229 233 223 255

corRelvaBase, corRelvaAlt, corTerraBase, corAguaBase :: Color
corRelvaBase = makeColorI 70 94 56 255
corRelvaAlt = makeColorI 81 108 66 255
corTerraBase = makeColorI 95 73 49 255
corAguaBase = makeColorI 67 112 133 255

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
  MostrarLojaMeta -> return $ desenhaLojaMeta e
  SelecionarModo -> return $ desenhaSelecionarModo e
  EditorMapa -> return $ desenhaEditorMapa e

-- ============================================================================
-- DESENHO DO JOGO EM MODO EMJOGO - ATUALIZADO
-- ============================================================================

desenhaJogoCompleto :: ImmutableTowers -> Picture
desenhaJogoCompleto e =
  Pictures [
    Color corFundoJogo $ rectangleSolid larguraJanela alturaJanela,
    Color (makeColorI 20 28 23 232) $ Translate 0 (-alturaJanela/2 + 66) $ rectangleSolid larguraJanela 132,
    Color (makeColorI 21 29 24 228) $ Translate 0 (alturaJanela/2 - 46) $ rectangleSolid larguraJanela 92,
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
    if hudCompacto e then Blank else desenhaPainelLateral e,
    if lojaVisivel e then desenhaLoja e else Blank,
    desenhaMensagens e,
    desenhaGameOver e,
    if hudCompacto e then Blank else desenhaInstrucoes e
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
      Text "Loja: clique para comprar | X alterna 1x/2x/4x | H/K escondem paineis"
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
    Color (withAlpha 0.3 (makeColorI 40 52 43 255)) $ Translate 0 78 $ rectangleSolid 960 650,
    Color (makeColorI 68 78 63 255) $ Translate 0 78 $ rectangleWire 960 650,
    Translate (-360) 300 $ Scale 0.42 0.42 $ Color corTextoSuave $ Text "IMMUTABLE",
    Translate (-286) 242 $ Scale 0.42 0.42 $ Color (makeColorI 226 194 95 255) $ Text "TOWERS",
    Translate (-360) 176 $ Scale 0.105 0.105 $ Color (makeColorI 154 164 146 255) $
      Text "Defende a base, evolui torres e compete por pontuacao.",
    Translate (-360) 140 $ Scale 0.09 0.09 $ Color corTextoSuave $
      Text ("Perfil: " ++ nomeJogador (perfilJogador e) ++ "   |   Modo: " ++ nomeModoJogo (modoJogoEscolhido e)),
    heroCard menuHeroRect (opcaoSel == Jogar) "JOGAR" "Entra logo na partida atual",
    heroCard menuModeRect (opcaoSel == Modos) "MODOS" "Escolhe mapa, ritmo e desafio",
    utilityCard menuShopRect (opcaoSel == LojaMeta) "LOJA",
    utilityCard menuProfileRect (opcaoSel == Perfil) "PERFIL",
    utilityCard menuLeaderboardRect (opcaoSel == Leaderboard) "RANKING",
    utilityCard menuHelpRect (opcaoSel == Creditos) "AJUDA",
    utilityCard menuOptionsRect (opcaoSel == Opcoes) "OPCOES",
    utilityCard menuExitRect (opcaoSel == Sair) "SAIR",
    painelResumoMenu e,
    Translate (-360) (-300) $ Scale 0.098 0.098 $ Color (makeColorI 154 164 146 255) $
      Text "Enter confirma | Setas navegam | E abre editor de mapa"
  ]
  where
    heroCard rect selecionado titulo subtitulo =
      let UIRect x y w h = rect
          corBorda = if selecionado then makeColorI 226 194 95 255 else makeColorI 92 103 88 255
          corFundo = if selecionado then makeColorI 55 60 45 235 else makeColorI 31 39 33 225
       in Pictures [
            Color corFundo $ Translate x y $ rectangleSolid w h,
            Color corBorda $ Translate x y $ rectangleWire w h,
            if selecionado then Color (withAlpha 0.35 corBorda) $ Translate x (y + h / 2 - 8) $ rectangleSolid (w - 34) 6 else Blank,
            Translate (x - w / 2 + 28) (y + 12) $ Scale 0.18 0.18 $ Color corTextoSuave $ Text titulo,
            Translate (x - w / 2 + 28) (y - 25) $ Scale 0.072 0.072 $ Color (makeColorI 154 164 146 255) $ Text subtitulo
          ]
    utilityCard rect selecionado titulo =
      drawButton (posicaoRato e) rect (if selecionado then Primary else Neutral) titulo

painelResumoMenu :: ImmutableTowers -> Picture
painelResumoMenu e =
  let x = 224
      y = 210
      linhas =
        [ capituloEstagioTexto (progressoMeta e),
          "Nivel " ++ show (nivelJogadorMeta (progressoMeta e)) ++ " | " ++ show (gemasJogador (progressoMeta e)) ++ " gemas",
          "Mapa rotativo: " ++ nomeMapa (mapaAtual e),
          "Torres desbloqueadas: " ++ show (length (torresDesbloqueadas (progressoMeta e)))
        ]
   in Pictures
        [ Color (withAlpha 0.9 corPainel) $ Translate x y $ rectangleSolid 340 236,
          Color (makeColorI 68 78 63 255) $ Translate x y $ rectangleWire 340 236,
          Translate (x - 136) (y + 70) $ Scale 0.125 0.125 $ Color (makeColorI 226 194 95 255) $ Text "RESUMO",
          Pictures
            [ Translate (x - 136) (y + 26 - fromIntegral i * 40) $
                Scale 0.082 0.082 $
                Color corTextoSuave $
                Text ("+ " ++ linha)
              | (i, linha) <- zip [0 :: Int ..] linhas
            ]
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
      meta = progressoMeta e
   in desenhaPainelTexto "PERFIL" [
        "Nome: " ++ nomeJogador perfil,
        "Nivel: " ++ show (nivelJogadorMeta meta),
        "Gemas: " ++ show (gemasJogador meta),
        "Jogos: " ++ show (jogosJogador perfil),
        "Vitorias: " ++ show (vitoriasJogador perfil),
        "Derrotas: " ++ show (derrotasJogador perfil),
        "Campanha: " ++ capituloEstagioTexto meta,
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
    modoCard e ModoHistoria (-360) 90 "HISTORIA" "capitulos com 5 estagios" "10 ondas por estagio",
    modoCard e ModoInfinito 0 90 "INFINITO" "ondas geradas sem fim" ("requer nivel " ++ show (nivelMinimoModo ModoInfinito)),
    modoCard e ModoDesafio 360 90 "DESAFIO" "ondas fortes mais cedo" ("requer nivel " ++ show (nivelMinimoModo ModoDesafio)),
    modoCard e ModoBoss (-180) (-105) "BOSS" "chefes e picos de dano" ("requer nivel " ++ show (nivelMinimoModo ModoBoss)),
    modoCard e ModoSandbox 180 (-105) "SANDBOX" "testar builds e upgrades" ("requer nivel " ++ show (nivelMinimoModo ModoSandbox)),
    Translate (-250) (-310) $ Scale 0.1 0.1 $ Color (makeColorI 154 164 146 255) $ Text "ENTER volta ao menu | modos bloqueados pedem nivel"
  ]

desenhaOpcoes :: ImmutableTowers -> Picture
desenhaOpcoes e =
  let rato = posicaoRato e
   in Pictures [
        Color corFundoJogo $ rectangleSolid larguraJanela alturaJanela,
        drawPanel (UIRect 0 40 820 590),
        Translate (-335) 270 $ Scale 0.3 0.3 $ Color (makeColorI 226 194 95 255) $ Text "OPCOES",
        Translate (-335) 214 $ Scale 0.095 0.095 $ Color corTextoSuave $ Text "Interface otimizada para 1920x1080 (Full HD)",
        Translate (-335) 156 $ Scale 0.095 0.095 $ Color corTextoSuave $ Text "Controlos principais:",
        Translate (-335) 102 $ Scale 0.083 0.083 $ Color (makeColorI 190 201 180 255) $ Text "Mouse: comprar, construir, selecionar, melhorar e vender torres",
        Translate (-335) 58 $ Scale 0.083 0.083 $ Color (makeColorI 190 201 180 255) $ Text "P pausa | X alterna 1x/2x/4x | S/L guarda/carrega | B sugestao bot",
        Translate (-335) 4 $ Scale 0.083 0.083 $ Color (makeColorI 190 201 180 255) $ Text "H recolhe HUD | K esconde loja | E abre editor de mapa | ESC volta",
        Translate (-335) (-76) $ Scale 0.095 0.095 $ Color corTextoSuave $ Text "Distribuicao:",
        Translate (-335) (-126) $ Scale 0.078 0.078 $ Color (makeColorI 190 201 180 255) $ Text "Assets atuais em app/imagens. Para release: copiar exe + pasta app/imagens.",
        Translate (-335) (-166) $ Scale 0.078 0.078 $ Color (makeColorI 190 201 180 255) $ Text "Windows: cabal build -O2 e empacotar dist-newstyle exe com DLLs necessarias.",
        drawButton rato (UIRect 0 (-245) 180 48) Neutral "ENTER / ESC"
      ]

desenhaLojaMeta :: ImmutableTowers -> Picture
desenhaLojaMeta e =
  let meta = progressoMeta e
      torresTexto =
        if null (torresDesbloqueadas meta)
          then "Sem torres desbloqueadas"
          else unwords (map nomeTowerId (take 8 (torresDesbloqueadas meta)))
   in Pictures
        [ Color corFundoJogo $ rectangleSolid larguraJanela alturaJanela,
          Color (withAlpha 0.96 corPainel) $ rectangleSolid 920 620,
          Color (makeColorI 68 78 63 255) $ rectangleWire 920 620,
          Translate (-380) 248 $ Scale 0.31 0.31 $ Color (makeColorI 226 194 95 255) $ Text "LOJA",
          Translate (-380) 198 $ Scale 0.1 0.1 $ Color corTextoSuave $ Text ("Gemas: " ++ show (gemasJogador meta) ++ "   Nivel: " ++ show (nivelJogadorMeta meta)),
          Translate (-380) 138 $ Scale 0.11 0.11 $ Color corTextoSuave $ Text "Baus",
          Translate (-380) 88 $ Scale 0.082 0.082 $ Color (makeColorI 190 201 180 255) $ Text ("1 Madeira " ++ show (custoBau BauMadeira) ++ " | 2 Cristal " ++ show (custoBau BauCristal) ++ " | 3 Imperial " ++ show (custoBau BauImperial)),
          Translate (-380) 16 $ Scale 0.11 0.11 $ Color corTextoSuave $ Text "Colecao",
          Translate (-380) (-38) $ Scale 0.076 0.076 $ Color (makeColorI 190 201 180 255) $ Text torresTexto,
          Translate (-380) (-118) $ Scale 0.11 0.11 $ Color corTextoSuave $ Text "Fusao",
          Translate (-380) (-170) $ Scale 0.08 0.08 $ Color (makeColorI 226 194 95 255) $ Text "F tecla: Tesla + Solar + 180 gemas -> Tempestade",
          Translate (-278) (-260) $ Scale 0.1 0.1 $ Color (makeColorI 154 164 146 255) $ Text "1/2/3 abrem baus | F funde | ESC / ENTER volta"
        ]

modoCard :: ImmutableTowers -> ModoJogoEscolhido -> Float -> Float -> String -> String -> String -> Picture
modoCard e modoCardAtual x y titulo desc regras =
  let selecionado = modoJogoEscolhido e == modoCardAtual
      desbloqueado = GameFactory.modoDesbloqueado (progressoMeta e) modoCardAtual
      fundo
        | not desbloqueado = makeColorI 24 26 25 220
        | selecionado = makeColorI 55 60 45 245
        | otherwise = makeColorI 25 34 29 230
      borda
        | not desbloqueado = makeColorI 72 73 69 255
        | selecionado = makeColorI 226 194 95 255
        | otherwise = makeColorI 85 102 82 255
   in Translate x y $ Pictures [
        Color fundo $ rectangleSolid 300 152,
        Color borda $ rectangleWire 300 152,
        if selecionado then Color (withAlpha 0.35 borda) $ Translate 0 70 $ rectangleSolid 270 6 else Blank,
        Translate (-120) 34 $ Scale 0.15 0.15 $ Color corTextoSuave $ Text titulo,
        Translate (-120) (-8) $ Scale 0.071 0.071 $ Color (makeColorI 190 201 180 255) $ Text desc,
        Translate (-120) (-42) $ Scale 0.069 0.069 $ Color (if desbloqueado then makeColorI 226 194 95 255 else makeColorI 190 82 72 255) $ Text regras,
        if modoCardAtual == ModoHistoria
          then Translate (-120) (-66) $ Scale 0.062 0.062 $ Color corTextoSuave $ Text (capituloEstagioTexto (progressoMeta e))
          else Blank
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
    Color (withAlpha 0.96 corPainel) $ rectangleSolid 860 590,
    Color (makeColorI 68 78 63 255) $ rectangleWire 860 590,
    Color (withAlpha 0.25 (makeColorI 226 194 95 255)) $ Translate 0 252 $ rectangleSolid 780 8,
    Translate (-338) 224 $ Scale 0.33 0.33 $ Color (makeColorI 226 194 95 255) $ Text titulo,
    Pictures [
      Translate (-338) (128 - fromIntegral i * 52) $ Scale 0.105 0.105 $ Color corTextoSuave $ Text linha
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
      cooldown = desenhaCooldownTorre (mapaJogo (jogo e)) torre
      foco = if torreFocada e == Just (posicaoTorre torre)
             then desenhaFocoTorre e torre
             else Blank
   in [alcance, foco, sprite, cooldown]

desenhaFocoTorre :: ImmutableTowers -> Torre -> Picture
desenhaFocoTorre e torre =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (posX, posY) = posicaoMapaParaEcra (mapaJogo (jogo e)) (posicaoTorre torre)
   in Color (makeColorI 226 194 95 255) $
        Translate posX posY $
        ThickCircle (bloco * 0.34) 3

desenhaAlcanceTorre :: Mapa -> Torre -> Maybe Torre -> Picture
desenhaAlcanceTorre mapa torre torreSel =
  let raio = alcanceTorre torre * calculaTamanhoBloco mapa
      (posX, posY) = posicaoMapaParaEcra mapa (posicaoTorre torre)
      -- Mostra alcance só se torre estiver selecionada
      alpha = case torreSel of
                Just t | posicaoTorre t == posicaoTorre torre -> 0.4
                _ -> 0
   in Color (withAlpha alpha (makeColorI 111 150 168 255)) $ 
        Translate posX posY $ 
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
      (posX, posY) = posicaoMapaParaEcra (mapaJogo (jogo e)) (posicaoTorre torre)
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
desenhaCooldownTorre :: Mapa -> Torre -> Picture
desenhaCooldownTorre mapa torre =
  let (posX, posYBase) = posicaoMapaParaEcra mapa (posicaoTorre torre)
      progresso = max 0 (1 - (tempoTorre torre / cicloTorre torre))
      largura = 30 * progresso
      posY = posYBase + 25
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
      vidaBar = desenhaBarraVida (mapaJogo (jogo e)) inimigo
   in Pictures [sprite, efeitos, vidaBar]

desenhaInimigoSprite :: ImmutableTowers -> Inimigo -> Picture
desenhaInimigoSprite e inimigo =
  let bloco = calculaTamanhoBloco (mapaJogo (jogo e))
      (posX, posY) = posicaoMapaParaEcra (mapaJogo (jogo e)) (posicaoInimigo inimigo)
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
      (posX, posY) = posicaoMapaParaEcra (mapaJogo (jogo e)) (posicaoInimigo inimigo)
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
desenhaBarraVida :: Mapa -> Inimigo -> Picture
desenhaBarraVida mapa inimigo =
  let (posX, posYBase) = posicaoMapaParaEcra mapa (posicaoInimigo inimigo)
      -- Assume vida máxima de 100 (ajustar se necessário)
      vidaMax = vidaMaxInimigoEstimado inimigo
      percentualVida = max 0 (min 1 (vidaInimigo inimigo / vidaMax))
      larguraTotal = 30
      larguraVida = larguraTotal * percentualVida
      posY = posYBase - 25
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
     in map (desenhaProjetil (mapaJogo (jogo e)) torre) inimigosAlvo
  else []

desenhaProjetil :: Mapa -> Torre -> Inimigo -> Picture
desenhaProjetil mapa torre inimigo =
  let (sx1, sy1) = posicaoMapaParaEcra mapa (posicaoTorre torre)
      (sx2, sy2) = posicaoMapaParaEcra mapa (posicaoInimigo inimigo)
      cor = case projetilTorre torre of
        Projetil Fogo _ -> makeColorI 175 88 58 255
        Projetil Gelo _ -> makeColorI 111 150 168 255
        Projetil Resina _ -> makeColorI 145 107 61 255
        Projetil Medo _ -> makeColorI 170 132 210 255
        Projetil Veneno _ -> makeColorI 101 168 92 255
        Projetil Eletrico _ -> makeColorI 226 194 95 255
   in Color cor $ Line [
        (sx1, sy1),
        (sx2, sy2)
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
      (posX, posY) = posicaoMapaParaEcra (mapaJogo (jogo e)) (posicaoPortal portal)
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
      (posX, posY) = posicaoMapaParaEcra (mapaJogo (jogo e)) (posicaoBase base)
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
      resumo = waveSummary e
      speed = show (round (velocidadeJogo e) :: Int) ++ "X"
      y = alturaJanela / 2 - 42
      painel = Color corPainel $ Translate 0 y $ rectangleSolid (larguraJanela - 72) 78
      vidaCor = if vida > 40 then makeColorI 135 174 111 255 else makeColorI 190 82 72 255
      ondaValor =
        if modoJogoEscolhido e == ModoInfinito
          then "W" ++ show (max 1 (ondaAtualUI resumo))
          else "W" ++ show (ondaAtualUI resumo) ++ "/" ++ show (ondasTotaisUI resumo)
      proximaTexto = case proximaOndaUI resumo of
        Just proxima -> "NEXT " ++ show proxima
        Nothing -> "LAST"
   in Pictures [
        painel,
        Color (makeColorI 65 75 61 255) $ Translate 0 y $ rectangleWire (larguraJanela - 72) 78,
        hudPill (-420) y "BASE" (show vida) vidaCor,
        hudPill (-220) y "CREDITOS" (show creditos) (makeColorI 226 194 95 255),
        hudPill 10 y "VAGA" ondaValor (makeColorI 226 153 76 255),
        hudPill 242 y "RESTAM" (show (inimigosRestantesUI resumo)) corTextoSuave,
        hudPill 456 y "VEL" speed (makeColorI 111 150 168 255),
        Translate (larguraJanela / 2 - 224) (y - 25) $ Scale 0.078 0.078 $ Color (makeColorI 154 164 146 255) $ Text ("Mapa: " ++ nomeMapa (mapaAtual e) ++ "   " ++ proximaTexto)
      ]

desenhaControlesJogo :: ImmutableTowers -> Picture
desenhaControlesJogo e =
  let rato = posicaoRato e
      speed = velocidadeJogo e
      toneSpeed alvo = if abs (speed - alvo) < 0.1 then Primary else Neutral
   in Pictures [
        drawButton rato startWaveRect Primary "INICIAR",
        drawButton rato pauseRect Neutral "||",
        drawButton rato speed1Rect (toneSpeed 1) "1X",
        drawButton rato speed2Rect (toneSpeed 2) "2X",
        drawButton rato speed4Rect (toneSpeed 4) "4X",
        drawButton rato hudToggleRect Neutral (if hudCompacto e then "HUD+" else "HUD-"),
        drawButton rato shopToggleRect Neutral (if lojaVisivel e then "SHOP-" else "SHOP+")
      ]

desenhaPainelLateral :: ImmutableTowers -> Picture
desenhaPainelLateral e =
  let base = baseJogo (jogo e)
      resumo = waveSummary e
      torreSel = torreSelecionada e
      torreAtiva = torreFocada e >>= \pos -> findTorreNaPosicao pos (torresJogo (jogo e))
      x = larguraJanela / 2 - 148
      y = 64
      linha n texto = Translate (x - 114) (y + 96 - fromIntegral n * 25) $
        Scale 0.08 0.08 $ Color corTextoSuave $ Text texto
      titulo = Translate (x - 114) (y + 132) $
        Scale 0.13 0.13 $ Color (makeColorI 226 194 95 255) $ Text "PAINEL"
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
        "Vaga: " ++ mostraVaga resumo (modoJogoEscolhido e),
        "Mapa: " ++ nomeMapa (mapaAtual e),
        "Restam: " ++ show (inimigosRestantesUI resumo) ++ " inimigos",
        "Campanha: " ++ capituloEstagioTexto (progressoMeta e),
        "Velocidade: " ++ show (round (velocidadeJogo e) :: Int) ++ "x"
        ] ++ [""] ++ infoTorre
   in Pictures [
        Color (withAlpha 0.88 corPainel) $ Translate x y $ rectangleSolid 262 362,
        Color (makeColorI 68 78 63 255) $ Translate x y $ rectangleWire 262 362,
        titulo,
        Pictures [linha i texto | (i, texto) <- zip [0 :: Int ..] linhas],
        drawButton (posicaoRato e) upgradeRect (maybe Disabled (const Primary) torreAtiva) "UP",
        drawButton (posicaoRato e) sellRect (maybe Disabled (const Danger) torreAtiva) "SELL",
        drawButton (posicaoRato e) cancelRect Neutral "X"
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
      torreSel = torreSelecionada e
      creditos = creditosBase (baseJogo (jogo e))
   in Pictures $
        [ Color (withAlpha 0.9 corPainel) $ Translate (-150) (-alturaJanela/2 + 66) $ rectangleSolid 760 108,
          Color (makeColorI 68 78 63 255) $ Translate (-150) (-alturaJanela/2 + 66) $ rectangleWire 760 108,
          Translate (-500) (-alturaJanela / 2 + 98) $ Scale 0.1 0.1 $ Color (makeColorI 154 164 146 255) $ Text ("ARSENAL  |  " ++ show (length loja) ++ " torres")
        ] ++ zipWith (desenhaBotaoLoja torreSel creditos) [0..] loja

desenhaBotaoLoja :: Maybe Torre -> Creditos -> Int -> (Creditos, Torre) -> Picture
desenhaBotaoLoja torreSel creditos indice (preco, torre) =
  let posX = -larguraJanela/2 + 82 + fromIntegral indice * 92
      posY = -alturaJanela/2 + 66
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
        Color corFundo $ rectangleSolid 82 96,
        Color cor $ rectangleWire 82 96,
        if selecionada then Color cor $ rectangleWire 86 100 else Blank,
        Translate 0 8 $ modeloTorre 42 (projetilTorre torre) selecionada,
        if compravel then Blank else Color (withAlpha 0.45 black) $ rectangleSolid 82 96,
        Translate (-30) (-30) $ Scale 0.052 0.052 $ Color corTextoSuave $ Text (nomeProjetilCurto (projetilTorre torre)),
        Translate 6 (-30) $ Scale 0.062 0.062 $ Color corPreco $ Text (show preco),
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

vidaMaxInimigoEstimado :: Inimigo -> Float
vidaMaxInimigoEstimado inimigo =
  case candidatos of
    vidaMax : _ -> max vidaMax (vidaInimigo inimigo)
    [] -> max 100 (vidaInimigo inimigo)
  where
    butim = butimInimigo inimigo
    ataque = ataqueInimigo inimigo
    candidatos =
      [ vidaOriginal nivel False
        | nivel <- niveisValidos (butim - 14),
          ataqueBaseCombina nivel
      ]
        ++ [ vidaOriginal nivel True
             | nivel <- niveisValidos (butim - 22),
               ataqueBrutoCombina nivel
           ]
    niveisValidos delta
      | delta < 0 = []
      | delta `mod` 4 == 0 = [delta `div` 4]
      | otherwise = []
    ataqueBaseCombina nivel =
      abs (ataque - (8 + fromIntegral nivel * 1.6)) < 0.2
    ataqueBrutoCombina nivel =
      abs (ataque - (14 + fromIntegral nivel * 1.6)) < 0.2
    vidaOriginal nivel bruto =
      let escala = fromIntegral nivel
       in 60 + escala * 24 + if bruto then escala * 26 else 0

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

hudPill :: Float -> Float -> String -> String -> Color -> Picture
hudPill x y etiqueta valor corValor =
  Translate x y $
    Pictures
      [ Color (withAlpha 0.24 corValor) $ rectangleSolid 164 48,
        Color (withAlpha 0.55 corValor) $ rectangleWire 164 48,
        Translate (-66) 6 $ Scale 0.085 0.085 $ Color (makeColorI 176 184 171 255) $ Text etiqueta,
        Translate (-14) (-6) $ Scale 0.16 0.16 $ Color corValor $ Text valor
      ]

mostraVaga :: WaveSummary -> ModoJogoEscolhido -> String
mostraVaga resumo modoAtual
  | modoAtual == ModoInfinito = show (max 1 (ondaAtualUI resumo))
  | otherwise = show (ondaAtualUI resumo) ++ "/" ++ show (ondasTotaisUI resumo)

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

posicaoMapaParaEcra :: Mapa -> Posicao -> (Float, Float)
posicaoMapaParaEcra terreno pos =
  fromMaybe (0, 0) (mapaParaEcra renderLayout terreno pos)

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

  (perfilGuardado, leaderboardGuardada, modoGuardado, metaGuardado) <- carregarMetaEstado

  let imgs = [
        (Fundo, fundo), (Grass, grass), (Water, water), (Land, land),
        (TorreResina, torreResina), (TorreGelo, torreGelo), (TorreFogo, torreFogo),
        (Inimigocima, inimigo), (BaseFoto, base), (PortalFoto, portal),
        (Play, playImg), (Exit, exitImg), (ButaoCreditos, creditosImg),
        (ImagemTutorial, tutorialImg), (ImagemCreditos, creditosImg)
        ]
      
      (jogoInicial, mapaInicial, totalOndas, metaInicialJogo) = prepararPartida modoGuardado metaGuardado
  
  return $ ImmutableTowers jogoInicial imgs (MenuInicial Jogar) 0 Nothing Nothing Nothing perfilGuardado leaderboardGuardada metaInicialJogo modoGuardado mapaInicial 0 totalOndas False 1 [] False True

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
