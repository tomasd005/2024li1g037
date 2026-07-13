module Desenhar where

import Data.Fixed (mod')
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import EnemySystem
import GameFactory
import Graphics.Gloss
import Graphics.Gloss.Juicy
import ImmutableTowers
import LI12425
import MapGeometry
import MenuComponents
import MetaTypes
import ProgressionSystem (capituloEstagioTexto)
import RenderConfig
import SaveSystem
import Tarefa2 (inimigosNoAlcance)
import TowerSystem
import TowerRuntime
import UIComponents
import UIRects
import UIState
import UIText

corFundoJogo, corPainel, corTextoSuave :: Color
corFundoJogo = makeColorI 30 43 34 255
corPainel = makeColorI 21 27 24 232
corTextoSuave = makeColorI 229 233 223 255

corRelvaBase, corRelvaAlt, corTerraBase, corAguaBase :: Color
corRelvaBase = makeColorI 70 94 56 255
corRelvaAlt = makeColorI 81 108 66 255
corTerraBase = makeColorI 95 73 49 255
corAguaBase = makeColorI 67 112 133 255

layoutRender :: ImmutableTowers -> MapLayoutConfig
layoutRender = layoutParaJanela . janelaAtual

viewW :: ImmutableTowers -> Float
viewW = fromIntegral . fst . janelaAtual

viewH :: ImmutableTowers -> Float
viewH = fromIntegral . snd . janelaAtual

uiScale :: ImmutableTowers -> Float
uiScale e = min (viewW e / larguraJanela) (viewH e / alturaJanela)

-- ============================================================================
-- FUNÇÃO PRINCIPAL DE DESENHO
-- ============================================================================

desenha :: ImmutableTowers -> IO Picture
desenha e = case modo e of
  MenuInicial opcao -> return $ desenhaMenu e opcao
  Pausado -> return $ desenhaPausado e
  EmJogo -> return $ desenhaJogoCompleto e
  TelaVitoria -> return $ desenhaResultadoPartida e
  TelaDerrota -> return $ desenhaResultadoPartida e
  TutorialFoto -> return $ desenhaTutorial e
  MostrarCreditos -> return $ desenhaCreditos e
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
    Color corFundoJogo $ rectangleSolid (viewW e) (viewH e),
    Scale (uiScale e) (uiScale e) $ Pictures
      [ Color (makeColorI 20 28 23 232) $ Translate 0 (-alturaJanela/2 + 34) $ rectangleSolid larguraJanela 68,
        Color (makeColorI 21 29 24 228) $ Translate 0 (alturaJanela/2 - 46) $ rectangleSolid larguraJanela 92,
    -- Camadas de baixo para cima
        mapaToPicture (layoutRender e) (imagens e) (mapaJogo (jogo e)),
        desenhaPreVisualizacaoColocacao e,
        desenhaTorres e,
        desenhaInimigosComEfeitos e,
        desenhaProjeteis e,
        desenhaEfeitosUpgrade e,
        desenhaPortais e,
        desenhaBase e (baseJogo (jogo e)),
        desenhaHUD e,
        desenhaControlesJogo e,
        if hudCompacto e then Blank else desenhaPainelLateral e,
        if lojaVisivel e then desenhaLoja e else Blank,
        desenhaMensagens e,
        if hudCompacto e then Blank else desenhaInstrucoes e
      ]
  ]

-- ============================================================================
-- DESENHO DE INSTRUÇÕES - NOVO
-- ============================================================================

desenhaInstrucoes :: ImmutableTowers -> Picture
desenhaInstrucoes e =
  if lojaVisivel e
    then Blank
    else case torreSelecionada e of
      Nothing ->
        drawGlossBody (-larguraJanela/2 + 286) (-alturaJanela/2 + 44) 0.064 corTextoSuave "Loja lateral  |  X alterna 1x 2x 4x  |  H e K escondem paineis"
      Just _ ->
        drawGlossBody (-larguraJanela/2 + 286) (-alturaJanela/2 + 44) 0.066 (makeColorI 235 194 96 255) "Clique na relva para colocar  |  Botao direito cancela"

-- ============================================================================
-- DESENHO DO MENU
-- ============================================================================

desenhaMenu :: ImmutableTowers -> MenuInicialOpcoes -> Picture
desenhaMenu e opcaoSel =
  Pictures [
    Color (makeColorI 18 28 24 255) $ rectangleSolid (viewW e) (viewH e),
    desenhaFundoAnimado e,
    Scale (uiScale e) (uiScale e) $ Pictures
      [ Color (withAlpha 0.5 (makeColorI 20 28 24 255)) $ Translate (-520) (-20) $ rectangleSolid 332 760,
        Color (makeColorI 73 87 74 255) $ Translate (-520) (-20) $ rectangleWire 332 760,
        drawGlossTitle (-635) 286 0.2 corTextoSuave "IMMUTABLE",
        drawGlossTitle (-597) 236 0.2 (makeColorI 226 194 95 255) "TOWERS",
        drawSidebarButton (posicaoRato e) menuHeroRect (opcaoSel == Jogar) "Jogar" "Entrar na partida",
        drawSidebarButton (posicaoRato e) menuModeRect (opcaoSel == Modos) "Modos" "Escolher desafio",
        drawSidebarButton (posicaoRato e) menuShopRect (opcaoSel == LojaMeta) "Loja" "Baús e gemas",
        drawSidebarButton (posicaoRato e) menuProfileRect (opcaoSel == Perfil) "Perfil" "Progressão local",
        drawSidebarButton (posicaoRato e) menuLeaderboardRect (opcaoSel == Leaderboard) "Ranking" "Melhores scores",
        drawSidebarButton (posicaoRato e) menuHelpRect (opcaoSel == Creditos) "Ajuda" "Como jogar",
        drawSidebarButton (posicaoRato e) menuOptionsRect (opcaoSel == Opcoes) "Opcoes" "Controlos e sistema",
        drawSidebarButton (posicaoRato e) menuExitRect (opcaoSel == Sair) "Sair" "Fechar o jogo",
        painelResumoMenu e,
        drawGlossBody (-634) (-446) 0.072 (makeColorI 180 188 174 255) ("Perfil: " ++ nomeJogador (perfilJogador e)),
        drawGlossBody (-634) (-474) 0.072 (makeColorI 180 188 174 255) ("Modo: " ++ nomeModoJogo (modoJogoEscolhido e)),
        drawTituloMenu (-152) 254 "Immutable" "Towers",
        drawSubtituloMenu (-152) 172 "Defende a base, evolui torres e compete por pontuacao.",
        drawBodyText (-152) 116 ("Campanha: " ++ capituloEstagioTexto (progressoMeta e)),
        drawBodyText (-152) 82 ("Nivel " ++ show (nivelJogadorMeta (progressoMeta e)) ++ " | " ++ show (gemasJogador (progressoMeta e)) ++ " gemas"),
        drawBodyText (-152) 48 ("Mapa atual: " ++ nomeMapa (mapaAtual e)),
        drawBodyText (-152) 14 ("Torres desbloqueadas: " ++ show (length (torresDesbloqueadas (progressoMeta e)))),
        drawSubtituloMenu (-152) (-56) "Enter confirma | Setas navegam | E abre editor de mapa",
        desenhaMensagens e
      ]
  ]

painelResumoMenu :: ImmutableTowers -> Picture
painelResumoMenu _ = Blank

desenhaTutorial :: ImmutableTowers -> Picture
desenhaTutorial e = desenhaPainelTextoAnimado e "TUTORIAL" [
    "1. Compra uma torre na loja no canto inferior esquerdo.",
    "2. Move o rato sobre a relva para ver a celula, alcance e validade.",
    "3. Clique esquerdo coloca a torre; botao direito cancela a selecao.",
    "4. Torres so podem ser colocadas em relva livre, nunca no caminho/agua.",
    "5. P pausa, U melhora torre, S/L guarda/carrega, B chama bot, O cria obstaculo."
  ]

desenhaCreditos :: ImmutableTowers -> Picture
desenhaCreditos e = desenhaPainelTextoAnimado e "CREDITOS" [
    "Immutable Towers",
    "Projeto LI1 — Tower Defense em Haskell + Gloss.",
    "UI, feedback de colocacao e compatibilidade Windows/GHCup atualizados.",
    "Pressiona ENTER ou ESC para voltar."
  ]

desenhaPerfil :: ImmutableTowers -> Picture
desenhaPerfil e =
  let perfil = perfilJogador e
      meta = progressoMeta e
   in desenhaPainelTextoAnimado e "PERFIL" [
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
   in desenhaPainelTextoAnimado e "LEADERBOARD" linhas
  where
    linhaScore n score = show n ++ ". " ++ nomePontuacao score
      ++ " | " ++ nomeModoJogo (modoPontuacao score)
      ++ " | " ++ show (valorPontuacao score)
      ++ " pts | ondas " ++ show (ondasPontuacao score)

desenhaSelecionarModo :: ImmutableTowers -> Picture
desenhaSelecionarModo e =
  Pictures [
    Color (makeColorI 18 28 24 255) $ rectangleSolid (viewW e) (viewH e),
    desenhaFundoAnimado e,
    Scale (uiScale e) (uiScale e) $ Pictures
      [ drawTituloMenu (-436) 298 "Modos" "",
        drawSubtituloMenu (-430) 226 ("Atual: " ++ nomeModoJogo (modoJogoEscolhido e) ++ " | Esquerda/direita ou clique"),
        modoCard e ModoHistoria (-360) 90 "HISTORIA" "CAPITULOS COM 5 ESTAGIOS" "10 ONDAS POR ESTAGIO",
        modoCard e ModoInfinito 0 90 "INFINITO" "ONDAS GERADAS SEM FIM" ("REQUER NIVEL " ++ show (nivelMinimoModo ModoInfinito)),
        modoCard e ModoDesafio 360 90 "DESAFIO" "ONDAS FORTES MAIS CEDO" ("REQUER NIVEL " ++ show (nivelMinimoModo ModoDesafio)),
        modoCard e ModoBoss (-180) (-105) "BOSS" "CHEFES E PICOS DE DANO" ("REQUER NIVEL " ++ show (nivelMinimoModo ModoBoss)),
        modoCard e ModoSandbox 180 (-105) "SANDBOX" "TESTAR BUILDS E UPGRADES" ("REQUER NIVEL " ++ show (nivelMinimoModo ModoSandbox)),
        drawButton (posicaoRato e) (UIRect 0 (-312) 150 48) Neutral "Voltar",
        desenhaMensagens e
      ]
  ]

desenhaOpcoes :: ImmutableTowers -> Picture
desenhaOpcoes e =
  let rato = posicaoRato e
      (rw, rh) = janelaAtual e
   in Pictures [
        Color (makeColorI 18 28 24 255) $ rectangleSolid (viewW e) (viewH e),
        desenhaFundoAnimado e,
        Scale (uiScale e) (uiScale e) $
          Pictures
            [ drawPanel (UIRect 0 20 820 650),
              drawTituloMenu (-336) 292 "Opcoes" "",
              drawSubtituloMenu (-335) 220 "Interface adaptada a diferentes tamanhos de janela",
              drawSectionText (-335) 160 "VIDEO",
              drawBodyText (-335) 122 ("Resolucao atual: " ++ show rw ++ "x" ++ show rh),
              drawBodyText (-335) 86 "Graficos: perfil Gloss 2D otimizado",
              drawSectionText (-335) 28 "CONTROLOS PRINCIPAIS",
              drawBodyText (-335) (-12) "Mouse: comprar, construir, selecionar, melhorar e vender torres",
              drawBodyText (-335) (-50) "P pausa | X alterna 1x/2x/4x | A bot automatico | B acao rapida",
              drawBodyText (-335) (-88) "S/L guarda e carrega | H recolhe HUD | K esconde loja",
              drawSectionText (-335) (-132) "DISTRIBUICAO",
              drawBodyText (-335) (-170) "Assets em app/imagens. Release = exe + pasta app/imagens.",
              drawBodyText (-335) (-208) "Windows: cabal build e empacotar o executavel com as DLLs necessarias.",
              drawButton rato optionsBackRect Neutral "Voltar",
              desenhaMensagens e
            ]
      ]

desenhaLojaMeta :: ImmutableTowers -> Picture
desenhaLojaMeta e =
  let meta = progressoMeta e
      torresTexto =
        if null (torresDesbloqueadas meta)
          then "Sem torres desbloqueadas"
          else unwords (map nomeTowerId (take 8 (torresDesbloqueadas meta)))
   in Pictures
        [ Color (makeColorI 18 28 24 255) $ rectangleSolid (viewW e) (viewH e),
          desenhaFundoAnimado e,
          Color (withAlpha 0.96 corPainel) $ rectangleSolid 920 620,
          Color (makeColorI 68 78 63 255) $ rectangleWire 920 620,
          drawGlossTitle (-380) 266 0.28 (makeColorI 226 194 95 255) "LOJA",
          drawGlossBody (-380) 214 0.11 corTextoSuave ("GEMAS: " ++ show (gemasJogador meta) ++ " | NIVEL: " ++ show (nivelJogadorMeta meta)),
          drawGlossTitle (-380) 150 0.14 corTextoSuave "BAUS",
          drawGlossBody (-380) 102 0.084 (makeColorI 190 201 180 255) ("1 MADEIRA " ++ show (custoBau BauMadeira) ++ " | 2 CRISTAL " ++ show (custoBau BauCristal) ++ " | 3 IMPERIAL " ++ show (custoBau BauImperial)),
          drawGlossTitle (-380) 30 0.14 corTextoSuave "COLECAO",
          drawGlossBody (-380) (-24) 0.078 (makeColorI 190 201 180 255) torresTexto,
          drawGlossTitle (-380) (-106) 0.14 corTextoSuave "FUSAO",
          drawGlossBody (-380) (-154) 0.082 (makeColorI 226 194 95 255) "F TECLA: TESLA + SOLAR + 180 GEMAS = TEMPESTADE",
          drawButton (posicaoRato e) shopBackRect Neutral "Voltar",
          drawGlossBody (-276) (-210) 0.078 (makeColorI 154 164 146 255) "1/2/3 ABREM BAUS | F FUNDE",
          desenhaMensagens e
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
        if selecionado then drawAccentBar 0 70 300 borda else Blank,
        drawGlossTitle (-118) 34 0.16 corTextoSuave titulo,
        drawGlossBody (-118) (-2) 0.072 (makeColorI 190 201 180 255) desc,
        drawGlossBody (-118) (-34) 0.066 (if desbloqueado then makeColorI 226 194 95 255 else makeColorI 190 82 72 255) regras,
        if modoCardAtual == ModoHistoria
          then drawGlossBody (-118) (-60) 0.058 corTextoSuave (capituloEstagioTexto (progressoMeta e))
          else Blank
      ]

desenhaFundoAnimado :: ImmutableTowers -> Picture
desenhaFundoAnimado e =
  let t = tempo e
      chuva =
        [ let px = fromIntegral (((i * 173) `mod` 1600) - 800)
              py = fromIntegral (((i * 97) `mod` 900) - 450)
              dy = (t * fromIntegral (18 + (i `mod` 9))) - fromIntegral ((i * 61) `mod` 900)
              y = py + realToFrac (mod' dy 900) - 450
              alpha = 0.05 + fromIntegral (i `mod` 4) * 0.02
           in Color (withAlpha alpha (makeColorI 196 210 182 255)) $
                Translate px y $
                rectangleSolid 2 24
        | i <- [0 :: Int .. 22]
        ]
      brilhos =
        [ Color (withAlpha 0.07 (makeColorI 88 120 102 255)) $
            Translate (220 + fromIntegral n * 40) (120 + sin (t * 0.42 + fromIntegral n) * 24) $
            rectangleSolid 520 78
        | n <- [0 :: Int .. 1]
        ]
      pulse = 56 + 8 * sin (t * 0.8)
   in Pictures
        [ Color (withAlpha 0.05 (makeColorI 226 194 95 255)) $ Translate 250 110 $ ThickCircle pulse 10
        , Color (withAlpha 0.04 (makeColorI 128 168 210 255)) $ Translate 390 (-70) $ ThickCircle (pulse * 0.78) 8
        , Pictures brilhos
        , Pictures chuva
        ]

drawTituloMenu :: Float -> Float -> String -> String -> Picture
drawTituloMenu x y linha1 linha2 =
  Pictures
    [ drawGlossTitle x y 0.36 corTextoSuave linha1,
      if null linha2 then Blank else drawGlossTitle (x + 78) (y - 62) 0.36 (makeColorI 226 194 95 255) linha2
    ]

drawSubtituloMenu :: Float -> Float -> String -> Picture
drawSubtituloMenu x y =
  drawGlossBody x y 0.105 (makeColorI 172 181 167 255)

drawSectionText :: Float -> Float -> String -> Picture
drawSectionText x y =
  drawGlossTitle x y 0.12 corTextoSuave

drawBodyText :: Float -> Float -> String -> Picture
drawBodyText x y =
  drawGlossBody x y 0.082 (makeColorI 198 205 192 255)

drawGlossTitle :: Float -> Float -> Float -> Color -> String -> Picture
drawGlossTitle x y s c txt =
  Pictures
    [ Translate (x + 2) (y - 2) $ Scale s s $ Color (withAlpha 0.28 black) $ Text txt,
      Translate x y $ Scale s s $ Color c $ Text txt
    ]

drawGlossBody :: Float -> Float -> Float -> Color -> String -> Picture
drawGlossBody x y s c txt =
  Pictures
    [ Translate (x + 1.5) (y - 1.5) $ Scale s s $ Color (withAlpha 0.22 black) $ Text txt,
      Translate x y $ Scale s s $ Color c $ Text txt
    ]

desenhaEditorMapa :: ImmutableTowers -> Picture
desenhaEditorMapa e =
  Pictures [
    Color corFundoJogo $ rectangleSolid (viewW e) (viewH e),
    mapaToPicture (layoutRender e) (imagens e) (mapaJogo (jogo e)),
    drawUITextLeft (-larguraJanela/2 + 44) (alturaJanela/2 - 40) 3 (makeColorI 226 194 95 255) "EDITOR DE MAPA",
    drawUITextLeft (-larguraJanela/2 + 44) (alturaJanela/2 - 74) 2 corTextoSuave "CLIQUE NUMA CELULA: RELVA -> TERRA -> ASFALTO -> AGUA. ESC VOLTA."
  ]

desenhaPainelTextoAnimado :: ImmutableTowers -> String -> [String] -> Picture
desenhaPainelTextoAnimado e titulo linhas =
  Pictures [
    Color (makeColorI 18 28 24 255) $ rectangleSolid (viewW e) (viewH e),
    desenhaFundoAnimado e,
    drawModalPanel 860 590 titulo linhas,
    drawButton (posicaoRato e) submenuBackRect Neutral "Voltar"
  ]

-- ============================================================================
-- DESENHO DE TORRES
-- ============================================================================

desenhaTorres :: ImmutableTowers -> Picture
desenhaTorres e =
  Pictures $ concatMap (desenhaTorreCompleta e) (torresJogo (jogo e))

desenhaTorreCompleta :: ImmutableTowers -> Torre -> [Picture]
desenhaTorreCompleta e torre =
  let cfg = layoutRender e
      alcance = desenhaAlcanceTorre cfg (mapaJogo (jogo e)) torre (torreSelecionada e)
      sprite = desenhaTorreSprite e torre
      cooldown = desenhaCooldownTorre cfg (mapaJogo (jogo e)) torre
      foco = if torreFocada e == Just (posicaoTorre torre)
             then desenhaFocoTorre e torre
             else Blank
   in [alcance, foco, sprite, cooldown]

desenhaFocoTorre :: ImmutableTowers -> Torre -> Picture
desenhaFocoTorre e torre =
  let cfg = layoutRender e
      bloco = calculaTamanhoBloco cfg (mapaJogo (jogo e))
      (posX, posY) = posicaoMapaParaEcra cfg (mapaJogo (jogo e)) (posicaoTorre torre)
   in Color (makeColorI 226 194 95 255) $
        Translate posX posY $
        ThickCircle (bloco * 0.34) 3

desenhaAlcanceTorre :: MapLayoutConfig -> Mapa -> Torre -> Maybe Torre -> Picture
desenhaAlcanceTorre cfg mapa torre torreSel =
  let raio = alcanceTorre torre * calculaTamanhoBloco cfg mapa
      (posX, posY) = posicaoMapaParaEcra cfg mapa (posicaoTorre torre)
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
      case ratoParaCelula (layoutRender e) (mapaJogo (jogo e)) rato of
        Just (cx, cy, centroX, centroY) ->
          let mapa = mapaJogo (jogo e)
              bloco = calculaTamanhoBloco (layoutRender e) mapa
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
                Color (withAlpha 0.55 corTextoSuave) $ Translate centroX centroY $ modeloTorre (TowerRuntime (fromMaybe (towerIdSpec (towerSpecAproximada torre)) (torreSelecionadaId e)) 1 Nothing) bloco True,
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
  let cfg = layoutRender e
      bloco = calculaTamanhoBloco cfg (mapaJogo (jogo e))
      (posX, posY) = posicaoMapaParaEcra cfg (mapaJogo (jogo e)) (posicaoTorre torre)
      runtime = towerRuntimeDaTorre (registoTorres e) torre
   in Translate posX posY $ modeloTorre runtime bloco False

modeloTorre :: TowerRuntime -> Float -> Bool -> Picture
modeloTorre runtime bloco selecionada =
  let corpo = makeColorI 66 72 69 255
      sombra = makeColorI 14 18 16 170
      metalClaro = makeColorI 186 192 184 255
      metalEscuro = makeColorI 38 43 41 255
      spec = towerSpec (runtimeTowerId runtime)
      raridade = raridadeTowerSpec spec
      (r, g, b) = corTowerSpec spec
      acento = makeColorI r g b 255
      basePlate = case raridade of
        Comum -> makeColorI 88 98 86 255
        Raro -> makeColorI 86 108 129 255
        Epico -> makeColorI 107 88 132 255
        Lendario -> makeColorI 146 106 58 255
        Mitico -> makeColorI 168 84 108 255
      nivelVisual = min 3 (max 0 (runtimeLevel runtime - 1))
      escala = bloco / 28
      aro = if selecionada
            then Color (makeColorI 226 194 95 255) $ rectangleWire (bloco * 0.92) (bloco * 0.92)
            else Blank
      topo = case formaTowerSpec spec of
        FormaSentinela -> Pictures [Color acento $ Translate 0 12 $ ThickCircle 4 4, Color metalClaro $ Translate 0 12 $ circleSolid 2]
        FormaCristal -> Pictures [
          Color acento $ Translate 0 12 $ Polygon [(-7,-3),(0,8),(7,-3),(0,-8)],
          Color metalClaro $ Translate 0 12 $ rectangleSolid 2 13
          ]
        FormaBraseiro -> Pictures [
          Color acento $ Translate 0 12 $ Polygon [(-7,-5),(0,9),(7,-5)],
          Color (makeColorI 226 194 95 255) $ Translate 0 12 $ Polygon [(-3,-2),(0,5),(3,-2)]
          ]
        FormaOrbe -> Pictures [Color acento $ Translate 0 12 $ ThickCircle 6 3, Color acento $ Translate 0 12 $ rectangleSolid 12 2]
        FormaFrasco -> Pictures [Color acento $ Translate 0 12 $ circleSolid 6, Color (withAlpha 0.55 metalClaro) $ Translate (-2) 14 $ circleSolid 2]
        FormaBobina -> Pictures [Color acento $ Translate 0 12 $ ThickCircle 5 3, Color metalClaro $ Translate 0 12 $ rectangleSolid 3 18]
        FormaCanhao -> Pictures [Color metalEscuro $ Translate 4 13 $ rectangleSolid 18 7, Color acento $ Translate (-7) 13 $ circleSolid 5]
        FormaSolar -> Pictures [Color acento $ Translate 0 12 $ circleSolid 5, Color (withAlpha 0.8 metalClaro) $ Translate 0 12 $ ThickCircle 9 2]
        FormaTempestade -> Pictures [Color acento $ Translate 0 12 $ ThickCircle 8 3, Color metalClaro $ Translate 0 12 $ Polygon [(-3,-8),(4,0),(-1,0),(4,8),(-6,1),(0,1)]]
      extraNivel1 =
        if nivelVisual >= 1
          then Pictures
            [ Color (withAlpha 0.8 acento) $ Translate (-7) 0 $ rectangleSolid 3 15
            , Color (withAlpha 0.8 acento) $ Translate 7 0 $ rectangleSolid 3 15
            ]
          else Blank
      extraNivel2 =
        if nivelVisual >= 2
          then Pictures
            [ Color metalClaro $ Translate 0 16 $ rectangleSolid 22 3
            , Color (withAlpha 0.75 acento) $ Translate 0 4 $ ThickCircle 8 2.4
            ]
          else Blank
      extraNivel3 =
        if nivelVisual >= 3
          then Pictures
            [ Color (withAlpha 0.28 acento) $ ThickCircle 15 4
            , Color (withAlpha 0.9 acento) $ Translate 0 18 $ Polygon [(-5,0),(0,5),(5,0),(0,-4)]
            ]
          else Blank
      extraEspecializacao = case runtimeSpecialization runtime of
        Nothing -> Blank
        Just EspecializacaoA ->
          Pictures
            [ Color (withAlpha 0.9 acento) $ Translate (-12) 2 $ Polygon [(-3,-8),(3,-8),(5,8),(-5,8)]
            , Color (withAlpha 0.9 acento) $ Translate 12 2 $ Polygon [(-3,-8),(3,-8),(5,8),(-5,8)]
            ]
        Just EspecializacaoB ->
          Pictures
            [ Color (withAlpha 0.9 acento) $ Translate (-11) 11 $ circleSolid 3
            , Color (withAlpha 0.9 acento) $ Translate 11 11 $ circleSolid 3
            , Color (withAlpha 0.55 acento) $ ThickCircle 13 2
            ]
      extraRaridade = case raridade of
        Comum -> Blank
        Raro ->
          Color (withAlpha 0.85 basePlate) $ Translate 0 (-13) $ rectangleSolid 24 4
        Epico ->
          Pictures
            [ Color (withAlpha 0.85 basePlate) $ Translate 0 (-13) $ rectangleSolid 24 4
            , Color (withAlpha 0.72 acento) $ Translate 0 10 $ ThickCircle 11 2
            ]
        Lendario ->
          Pictures
            [ Color (withAlpha 0.88 basePlate) $ Translate 0 (-13) $ rectangleSolid 26 5
            , Color (withAlpha 0.82 acento) $ Translate (-10) 3 $ rectangleSolid 3 18
            , Color (withAlpha 0.82 acento) $ Translate 10 3 $ rectangleSolid 3 18
            ]
        Mitico ->
          Pictures
            [ Color (withAlpha 0.9 basePlate) $ Translate 0 (-13) $ rectangleSolid 28 5
            , Color (withAlpha 0.24 acento) $ ThickCircle 18 5
            , Color (withAlpha 0.82 acento) $ Translate 0 18 $ Polygon [(-7,0),(0,7),(7,0),(0,-5)]
            ]
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
          topo,
          extraRaridade,
          extraNivel1,
          extraNivel2,
          extraNivel3,
          extraEspecializacao
        ]
      ]

-- Barra de cooldown acima da torre
desenhaCooldownTorre :: MapLayoutConfig -> Mapa -> Torre -> Picture
desenhaCooldownTorre cfg mapa torre =
  let (posX, posYBase) = posicaoMapaParaEcra cfg mapa (posicaoTorre torre)
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
  let cfg = layoutRender e
      sprite = desenhaInimigoSprite e inimigo
      efeitos = desenhaEfeitosInimigo e inimigo
      vidaBar = desenhaBarraVida cfg (mapaJogo (jogo e)) inimigo
   in Pictures [sprite, efeitos, vidaBar]

desenhaInimigoSprite :: ImmutableTowers -> Inimigo -> Picture
desenhaInimigoSprite e inimigo =
  let cfg = layoutRender e
      bloco = calculaTamanhoBloco cfg (mapaJogo (jogo e))
      (posX, posY) = posicaoMapaParaEcra cfg (mapaJogo (jogo e)) (posicaoInimigo inimigo)
   in Translate posX posY $ modeloInimigo bloco inimigo

modeloInimigo :: Float -> Inimigo -> Picture
modeloInimigo bloco inimigo =
  let spec = enemySpec (enemyClassOf inimigo)
      (r, g, b) = corEnemySpec spec
      pele = makeColorI r g b 255
      contorno = makeColorI 39 31 29 255
      olho = makeColorI 222 198 118 255
      tamanho = case formaEnemySpec spec of
        FormaVeloz -> 0.82
        FormaPesada -> 1.14
        FormaBlindada -> 1.12
        FormaElite -> 1.16
        FormaBossA -> 1.38
        FormaBossG -> 1.48
        FormaBossR -> 1.42
        _ -> 1
      escala = bloco / 24 * tamanho
      corpo = case formaEnemySpec spec of
        FormaVeloz -> Polygon [(-10,-6),(8,-8),(11,0),(8,8),(-10,6),(-6,0)]
        FormaPesada -> rectangleSolid 19 17
        FormaBlindada -> Polygon [(-10,-6),(-6,-10),(6,-10),(10,-6),(10,6),(6,10),(-6,10),(-10,6)]
        FormaRegeneradora -> ThickCircle 7 5
        FormaDispersora -> Polygon [(0,10),(9,3),(6,-9),(-6,-9),(-9,3)]
        FormaEscudo -> Polygon [(0,11),(10,5),(8,-8),(0,-11),(-8,-8),(-10,5)]
        FormaElite -> Polygon [(0,11),(10,0),(7,-9),(-7,-9),(-10,0)]
        FormaBossA -> Polygon [(-12,-7),(9,-10),(13,0),(9,10),(-12,7),(-7,0)]
        FormaBossG -> rectangleSolid 22 20
        FormaBossR -> ThickCircle 8 7
        FormaRedonda -> circleSolid 9
      detalhe = case formaEnemySpec spec of
        FormaBlindada -> Color (withAlpha 0.8 (makeColorI 207 214 211 255)) $ rectangleWire 16 14
        FormaRegeneradora -> Color (makeColorI 174 225 166 255) $ rectangleSolid 3 13
        FormaDispersora -> Color (withAlpha 0.75 white) $ ThickCircle 11 2
        FormaEscudo -> Color (withAlpha 0.7 (makeColorI 132 205 235 255)) $ ThickCircle 12 3
        FormaElite -> Color (makeColorI 235 194 96 255) $ Translate 0 9 $ Polygon [(-4,0),(0,5),(4,0)]
        FormaBossA -> Color (makeColorI 238 173 92 255) $ Translate (-11) 0 $ Polygon [(-5,-4),(1,0),(-5,4)]
        FormaBossG -> Color (makeColorI 180 210 224 255) $ rectangleWire 20 18
        FormaBossR -> Color (makeColorI 225 151 235 255) $ ThickCircle 13 3
        _ -> Blank
   in Scale escala escala $ Pictures [
        Color (withAlpha 0.35 black) $ Translate 2 (-3) corpo,
        Color contorno corpo,
        Scale 0.82 0.82 $ Color pele corpo,
        detalhe,
        Color olho $ Translate (-3) 2 $ circleSolid 1.6,
        Color olho $ Translate 3 2 $ circleSolid 1.6,
        Color contorno $ Translate 0 (-3) $ rectangleSolid 8 2
      ]

-- Efeitos visuais de projéteis ativos
desenhaEfeitosInimigo :: ImmutableTowers -> Inimigo -> Picture
desenhaEfeitosInimigo e inimigo =
  let cfg = layoutRender e
      bloco = calculaTamanhoBloco cfg (mapaJogo (jogo e))
      (posX, posY) = posicaoMapaParaEcra cfg (mapaJogo (jogo e)) (posicaoInimigo inimigo)
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
      classe = enemyClassOf inimigo
      pulso = 0.78 + 0.22 * sin (tempo e * 2.4)
      auraGuardiao = if classe == BossGuardiao
        then Color (withAlpha 0.38 (makeColorI 132 205 235 255)) $
               Translate posX posY $ ThickCircle (bloco * raioAuraGuardiao * pulso) 2.4
        else Blank
      zonaRuptura = if classe == BossRuptura
        then Translate posX posY $ Pictures
               [ Color (withAlpha 0.24 (makeColorI 225 151 235 255)) $ ThickCircle (bloco * raioZonaRuptura) 2.2
               , Color (withAlpha 0.16 (makeColorI 225 151 235 255)) $ ThickCircle (bloco * raioZonaRuptura * 0.72 * pulso) 1.6
               ]
        else Blank
   
   in Pictures [auraGuardiao, zonaRuptura, efeitoFogo, efeitoGelo, efeitoResina, efeitoMedo, efeitoVeneno, efeitoEletrico]

-- Barra de vida acima do inimigo
desenhaBarraVida :: MapLayoutConfig -> Mapa -> Inimigo -> Picture
desenhaBarraVida cfg mapa inimigo =
  let (posX, posYBase) = posicaoMapaParaEcra cfg mapa (posicaoInimigo inimigo)
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

desenhaEfeitosUpgrade :: ImmutableTowers -> Picture
desenhaEfeitosUpgrade e =
  let cfg = layoutRender e
      mapa = mapaJogo (jogo e)
   in Pictures (map (desenhaEfeitoUpgrade cfg mapa) (efeitosUpgrade e))

desenhaEfeitoUpgrade :: MapLayoutConfig -> Mapa -> EfeitoUpgradeUI -> Picture
desenhaEfeitoUpgrade cfg mapa efeito =
  let t = max 0 (min 1 (tempoEfeitoUpgrade efeito / 0.65))
      bloco = calculaTamanhoBloco cfg mapa
      raio = bloco * (0.34 + (1 - t) * 0.38)
      espessura = max 2 (bloco * 0.05 * t + 1.8)
      alpha = 0.18 + t * 0.5
      (x, y) = posicaoMapaParaEcra cfg mapa (posicaoEfeitoUpgrade efeito)
   in Translate x y $
        Pictures
          [ Color (withAlpha alpha (makeColorI 226 194 95 255)) $ ThickCircle raio espessura,
            Color (withAlpha (alpha * 0.7) (makeColorI 239 234 186 255)) $ ThickCircle (raio * 0.72) (max 1.5 (espessura * 0.58))
          ]

desenhaProjeteisdeTorre :: ImmutableTowers -> Torre -> [Picture]
desenhaProjeteisdeTorre e torre =
  -- Só desenha projéteis se torre acabou de disparar (cooldown alto)
  if tempoTorre torre > cicloTorre torre * 0.8
  then
    let inimigosAlvo = take (rajadaTorre torre) (inimigosNoAlcance torre (inimigosJogo (jogo e)))
        cfg = layoutRender e
        fase = if cicloTorre torre <= 0 then 0 else (tempoTorre torre - cicloTorre torre * 0.8) / (cicloTorre torre * 0.2)
     in concatMap (desenhaProjetilComDano cfg (mapaJogo (jogo e)) torre fase) inimigosAlvo
  else []

desenhaProjetilComDano :: MapLayoutConfig -> Mapa -> Torre -> Float -> Inimigo -> [Picture]
desenhaProjetilComDano cfg mapa torre fase inimigo =
  [desenhaProjetil cfg mapa torre inimigo, desenhaNumeroDano cfg mapa torre inimigo fase]

desenhaProjetil :: MapLayoutConfig -> Mapa -> Torre -> Inimigo -> Picture
desenhaProjetil cfg mapa torre inimigo =
  let (sx1, sy1) = posicaoMapaParaEcra cfg mapa (posicaoTorre torre)
      (sx2, sy2) = posicaoMapaParaEcra cfg mapa (posicaoInimigo inimigo)
      cor = case projetilTorre torre of
        Projetil Fogo _ -> makeColorI 175 88 58 255
        Projetil Gelo _ -> makeColorI 111 150 168 255
        Projetil Resina _ -> makeColorI 145 107 61 255
        Projetil Medo _ -> makeColorI 170 132 210 255
        Projetil Veneno _ -> makeColorI 101 168 92 255
        Projetil Eletrico _ -> makeColorI 226 194 95 255
      spark =
        Translate sx2 sy2 $
          Pictures
            [ Color (withAlpha 0.82 cor) $ ThickCircle 5 2.2
            , Color (withAlpha 0.9 (makeColorI 241 236 214 255)) $ Line [(-6,0),(6,0)]
            , Color (withAlpha 0.9 (makeColorI 241 236 214 255)) $ Line [(0,-6),(0,6)]
            ]
   in Pictures
        [ Color cor $ Line [(sx1, sy1), (sx2, sy2)]
        , spark
        ]

desenhaNumeroDano :: MapLayoutConfig -> Mapa -> Torre -> Inimigo -> Float -> Picture
desenhaNumeroDano cfg mapa torre inimigo fase =
  let (sx, sy) = posicaoMapaParaEcra cfg mapa (posicaoInimigo inimigo)
      subida = (1 - max 0 (min 1 fase)) * 18
      danoBase = max 1 (floor (danoTorre torre) :: Int)
      cor =
        case tipoProjetil (projetilTorre torre) of
          Fogo -> makeColorI 241 156 118 255
          Gelo -> makeColorI 191 225 241 255
          Resina -> makeColorI 210 178 126 255
          Medo -> makeColorI 218 188 245 255
          Veneno -> makeColorI 173 223 154 255
          Eletrico -> makeColorI 248 224 123 255
   in drawUITextCentered sx (sy + 22 + subida) 2.3 cor ("-" ++ show danoBase)

-- ============================================================================
-- DESENHO DE PORTAIS
-- ============================================================================

desenhaPortais :: ImmutableTowers -> Picture
desenhaPortais e =
  Pictures $ map (desenhaPortal e) (portaisJogo (jogo e))

desenhaPortal :: ImmutableTowers -> Portal -> Picture
desenhaPortal e portal =
  let cfg = layoutRender e
      bloco = calculaTamanhoBloco cfg (mapaJogo (jogo e))
      (posX, posY) = posicaoMapaParaEcra cfg (mapaJogo (jogo e)) (posicaoPortal portal)
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
  let cfg = layoutRender e
      bloco = calculaTamanhoBloco cfg (mapaJogo (jogo e))
      (posX, posY) = posicaoMapaParaEcra cfg (mapaJogo (jogo e)) (posicaoBase base)
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
      painel = Color corPainel $ Translate 0 y $ rectangleSolid (larguraJanela - 48) 82
      vidaCor = if vida > 40 then makeColorI 135 174 111 255 else makeColorI 190 82 72 255
      ondaValor =
        if modoJogoEscolhido e == ModoInfinito
          then "W" ++ show (max 1 (ondaAtualUI resumo))
          else "W" ++ show (ondaAtualUI resumo) ++ "/" ++ show (ondasTotaisUI resumo)
   in Pictures [
        painel,
        Color (makeColorI 65 75 61 255) $ Translate 0 y $ rectangleWire (larguraJanela - 48) 82,
        hudPill (-664) y 154 "BASE" (show vida) vidaCor,
        hudPill (-486) y 154 "CREDITOS" (show creditos) (makeColorI 226 194 95 255),
        hudPill (-308) y 154 "VAGA" ondaValor (makeColorI 226 153 76 255),
        hudPill (-130) y 154 "RESTAM" (show (inimigosRestantesUI resumo)) corTextoSuave,
        hudPill 48 y 154 "VEL" speed (makeColorI 111 150 168 255),
        hudPill 226 y 154 "BOT" (if botAutomatico e then "AUTO" else "MANUAL") (if botAutomatico e then makeColorI 135 174 111 255 else makeColorI 154 164 146 255)
      ]

desenhaControlesJogo :: ImmutableTowers -> Picture
desenhaControlesJogo e =
  let rato = posicaoRato e
      speed = velocidadeJogo e
      toneSpeed alvo = if abs (speed - alvo) < 0.1 then Primary else Neutral
   in Pictures [
        drawButton rato pauseRect Neutral "||",
        drawButton rato speed1Rect (toneSpeed 1) "1X",
        drawButton rato speed2Rect (toneSpeed 2) "2X",
        drawButton rato speed4Rect (toneSpeed 4) "4X",
        drawButton rato autoBotRect (if botAutomatico e then Primary else Neutral) "AUTO",
        drawButton rato hudToggleRect Neutral "HUD",
        drawButton rato shopToggleRect Neutral "LOJA"
      ]

desenhaPainelLateral :: ImmutableTowers -> Picture
desenhaPainelLateral e =
  let base = baseJogo (jogo e)
      resumo = waveSummary e
      torreSel = torreSelecionada e
      torreAtiva = torreFocada e >>= \pos -> findTorreNaPosicao pos (torresJogo (jogo e))
      UIRect x y panelW panelH = gamePanelRect
      top = y + panelH / 2
      secX = x - panelW / 2 + 30
      stats =
        [ ("Vida", show (floor (vidaBase base) :: Int))
        , ("Creditos", show (creditosBase base))
        , ("Modo", nomeModoJogo (modoJogoEscolhido e))
        , ("Vaga", mostraVaga resumo (modoJogoEscolhido e))
        , ("Restam", show (inimigosRestantesUI resumo))
        , ("Vel", show (round (velocidadeJogo e) :: Int) ++ "x")
        ]
      drawStat i (rotulo, valor) =
        let rowY = top - 170 - fromIntegral i * 28
         in Pictures
              [ drawGlossBody secX rowY 0.07 (makeColorI 154 164 146 255) rotulo
              , drawGlossBody (secX + 104) rowY 0.076 corTextoSuave valor
              ]
      towerHeader = drawGlossBody secX (top - 354) 0.07 (makeColorI 154 164 146 255) "TORRE"
      towerInfo = case torreAtiva of
        Just torre ->
          let runtime = towerRuntimeDaTorre (registoTorres e) torre
              spec = towerSpec (runtimeTowerId runtime)
              previa = upgradeTorreRuntime runtime torre
              custo = custoUpgradeRuntime runtime torre
              upgradeHover =
                maybe
                  False
                  (\pos -> any (containsPoint pos) [upgradeRect, specializationARect, specializationBRect])
                  (posicaoRato e)
              corPreview = if upgradeHover then makeColorI 239 226 153 255 else makeColorI 189 198 180 255
              dps = danoTorre torre * fromIntegral (rajadaTorre torre) / max 0.1 (cicloTorre torre)
              venda = valorVendaRuntime runtime torre
              previewLinhas
                | precisaEspecializacao runtime =
                    let custoA = fromMaybe 0 (custoEspecializacao EspecializacaoA runtime torre)
                        custoB = fromMaybe 0 (custoEspecializacao EspecializacaoB runtime torre)
                     in [ drawGlossBody secX (top - 526) 0.06 (makeColorI 239 226 153 255) ("DANO+  dano/alcance  " ++ show custoA)
                        , drawGlossBody secX (top - 552) 0.06 (makeColorI 168 205 226 255) ("RAPIDA  rajada/ciclo  " ++ show custoB)
                        , drawGlossBody secX (top - 578) 0.058 corTextoSuave "Escolha permanente"
                        ]
                | otherwise =
                    case (previa, custo) of
                      (Just torreNova, Just custoNovo) ->
                        [ drawGlossBody secX (top - 526) 0.062 corPreview ("+" ++ show (floor (danoTorre torreNova - danoTorre torre) :: Int) ++ " dano  +" ++ showDeltaFloat (alcanceTorre torreNova - alcanceTorre torre) ++ " alcance")
                        , drawGlossBody secX (top - 552) 0.062 corPreview ("Ciclo " ++ showTempoCurto (cicloTorre torre) ++ " -> " ++ showTempoCurto (cicloTorre torreNova))
                        , drawGlossBody secX (top - 578) 0.062 corPreview ("Nivel " ++ show (runtimeLevel runtime + 1) ++ "  |  " ++ show custoNovo ++ " cred")
                        ]
                      _ -> [drawGlossBody secX (top - 548) 0.072 (makeColorI 226 194 95 255) "NIVEL MAXIMO"]
           in [ drawGlossTitle secX (top - 386) 0.105 (makeColorI 226 194 95 255) (nomeTowerSpec spec)
              , drawGlossBody secX (top - 412) 0.06 (corRaridade (raridadeTowerSpec spec)) (nomeRaridadeUI (raridadeTowerSpec spec) ++ "  |  NIVEL " ++ show (runtimeLevel runtime) ++ "/" ++ show (nivelMaximoTowerSpec spec))
              , drawGlossBody secX (top - 442) 0.066 corTextoSuave ("Dano " ++ show (floor $ danoTorre torre :: Int) ++ "  |  DPS ~" ++ show (floor dps :: Int))
              , drawGlossBody secX (top - 470) 0.066 corTextoSuave ("Alcance " ++ show (floor $ alcanceTorre torre :: Int) ++ "  |  Ciclo " ++ showTempoCurto (cicloTorre torre))
              , drawGlossBody secX (top - 498) 0.06 corTextoSuave (nomePrioridade (prioridadeTowerSpec spec) ++ "  |  Venda " ++ show venda)
              ] ++ previewLinhas
        Nothing -> case torreSel of
          Just torre ->
            let spec = towerSpec (fromMaybe (towerIdSpec (towerSpecAproximada torre)) (torreSelecionadaId e))
             in [ drawGlossTitle secX (top - 386) 0.105 (makeColorI 226 194 95 255) ("Comprar " ++ nomeTowerSpec spec)
                , drawGlossBody secX (top - 412) 0.06 (corRaridade (raridadeTowerSpec spec)) (nomeRaridadeUI (raridadeTowerSpec spec) ++ "  |  " ++ nomeProjetilCurto (projetilTorre torre))
                , drawGlossBody secX (top - 442) 0.066 corTextoSuave ("Dano " ++ show (floor $ danoTorre torre :: Int) ++ "  |  Alcance " ++ show (floor $ alcanceTorre torre :: Int))
                , drawGlossBody secX (top - 470) 0.066 corTextoSuave ("Rajada " ++ show (rajadaTorre torre) ++ "  |  Efeito " ++ showDuracaoProjetil (projetilTorre torre))
                , drawGlossBody secX (top - 500) 0.066 corTextoSuave "Clique na relva para colocar"
                ]
          Nothing ->
            [ drawGlossTitle secX (top - 386) 0.105 (makeColorI 176 184 171 255) "Sem selecao"
            , drawGlossBody secX (top - 422) 0.066 corTextoSuave "Escolhe uma torre do arsenal"
            , drawGlossBody secX (top - 450) 0.066 corTextoSuave "ou clica numa torre no mapa"
            ]
      botoesPainel = case (torreAtiva, torreSel) of
        (Just torre, _) ->
          let runtime = towerRuntimeDaTorre (registoTorres e) torre
              upgradeTone = if runtimeLevel runtime >= nivelMaximoTowerSpec (towerSpec (runtimeTowerId runtime)) then Disabled else Primary
           in ( if precisaEspecializacao runtime
                  then
                    [ drawButton (posicaoRato e) specializationARect Primary "DANO+"
                    , drawButton (posicaoRato e) specializationBRect Neutral "RAPIDA"
                    ]
                  else [drawButton (posicaoRato e) upgradeRect upgradeTone "UPGRADE"]
              )
                ++ [ drawButton (posicaoRato e) sellRect Danger "VENDER"
                   , drawButton (posicaoRato e) cancelRect Neutral "LIMPAR"
                   ]
        (Nothing, Just _) ->
          [ drawButton (posicaoRato e) cancelRect Neutral "LIMPAR" ]
        _ -> []
   in Pictures [
        Color (withAlpha 0.94 corPainel) $ Translate x y $ rectangleSolid panelW panelH,
        Color (makeColorI 68 78 63 255) $ Translate x y $ rectangleWire panelW panelH,
        drawGlossTitle secX (top - 44) 0.125 (makeColorI 226 194 95 255) "PAINEL",
        drawGlossBody secX (top - 76) 0.06 corTextoSuave ("MAPA  " ++ nomeMapa (mapaAtual e)),
        drawGlossBody secX (top - 102) 0.06 (makeColorI 154 164 146 255) (capituloEstagioTexto (progressoMeta e)),
        drawGlossBody secX (top - 130) 0.058 (makeColorI 226 194 95 255) (resumoProximaVaga resumo),
        Pictures [drawStat i item | (i, item) <- zip [0 :: Int ..] stats],
        Color (withAlpha 0.16 (makeColorI 176 184 171 255)) $ Translate x (top - 330) $ rectangleSolid (panelW - 56) 2,
        towerHeader,
        Pictures towerInfo,
        Pictures botoesPainel
      ]

resumoProximaVaga :: WaveSummary -> String
resumoProximaVaga resumo =
  let numero = fromMaybe (ondaAtualUI resumo) (proximaOndaUI resumo)
      entradas = composicaoProximaUI resumo
      primeiras = take 3 entradas
      sufixo = if length entradas > 3 then "  +" else ""
      grupos = intercalate "  " [show quantidade ++ " " ++ siglaEnemy enemyClass | (enemyClass, quantidade) <- primeiras]
   in if null entradas
        then "PROX  --"
        else "PROX W" ++ show numero ++ "  " ++ grupos ++ sufixo

siglaEnemy :: EnemyClass -> String
siglaEnemy enemyClass = case enemyClass of
  Basico -> "BAS"
  Rapido -> "RAP"
  Tanque -> "TAN"
  Blindado -> "BLD"
  Regenerador -> "REG"
  Dispersor -> "DSP"
  Protegido -> "ESC"
  Elite -> "ELT"
  BossAcelerador -> "B-A"
  BossGuardiao -> "B-G"
  BossRuptura -> "B-R"

findTorreNaPosicao :: Posicao -> [Torre] -> Maybe Torre
findTorreNaPosicao _ [] = Nothing
findTorreNaPosicao pos (torre:torres)
  | posicaoTorre torre == pos = Just torre
  | otherwise = findTorreNaPosicao pos torres

nomePrioridade :: TargetPriority -> String
nomePrioridade prioridade = case prioridade of
  PrimeiroNaRota -> "ALVO PRIMEIRO"
  MaisRapido -> "ALVO RAPIDO"
  MaisVida -> "ALVO RESISTENTE"
  MaiorGrupo -> "ALVO GRUPO"

-- ============================================================================
-- LOJA (SHOP) - ATUALIZADA
-- ============================================================================

desenhaLoja :: ImmutableTowers -> Picture
desenhaLoja e =
  let loja = shopEntriesParaModo (modoJogoEscolhido e) (progressoMeta e)
      torreSelId = torreSelecionadaId e
      creditos = creditosBase (baseJogo (jogo e))
      UIRect panelX panelY panelW panelH = shopPanelRect (length loja)
      headerY = panelY + panelH / 2 - 34
   in Pictures $
        [ Color (withAlpha 0.94 corPainel) $ Translate panelX panelY $ rectangleSolid panelW panelH,
          Color (makeColorI 68 78 63 255) $ Translate panelX panelY $ rectangleWire panelW panelH,
          drawGlossTitle (panelX - panelW / 2 + 20) headerY 0.095 (makeColorI 226 194 95 255) "ARSENAL",
          drawGlossBody (panelX - panelW / 2 + 20) (headerY - 28) 0.062 corTextoSuave ("Creditos " ++ show creditos),
          drawGlossBody (panelX - panelW / 2 + 130) (headerY - 28) 0.058 (makeColorI 154 164 146 255) (show (length loja) ++ " torres"),
          Color (withAlpha 0.16 (makeColorI 176 184 171 255)) $ Translate panelX (headerY - 42) $ rectangleSolid (panelW - 32) 2
        ] ++ zipWith (desenhaBotaoLoja (length loja) torreSelId creditos) [0..] loja

desenhaBotaoLoja :: Int -> Maybe TowerId -> Creditos -> Int -> ShopEntry -> Picture
desenhaBotaoLoja total towerIdSelecionada creditos indice entrada =
  let (posX, posY) = shopSlotCenter total indice
      towerId = shopTowerId entrada
      preco = shopPrice entrada
      torre = shopTower entrada
      spec = towerSpec towerId
      compravel = creditos >= preco
      selecionada = towerIdSelecionada == Just towerId
      cor = if selecionada then makeColorI 226 194 95 255 else if compravel then makeColorI 92 103 88 255 else makeColorI 78 70 66 255
      corFundo = if selecionada 
                 then makeColorI 55 60 45 235
                 else if compravel then makeColorI 31 39 33 225 else makeColorI 28 28 27 215
      corPreco = if compravel then makeColorI 226 194 95 255 else makeColorI 190 82 72 255
      textoInfo = if selecionada 
                  then Pictures [
                    drawGlossBody (-31) 29 0.046 corTextoSuave ("A " ++ show (floor $ alcanceTorre torre :: Int)),
                    drawGlossBody (-31) 12 0.046 corTextoSuave ("D " ++ show (floor $ danoTorre torre :: Int))
                  ]
                  else Blank
   in Translate posX posY $ Pictures [
        Color corFundo $ rectangleSolid 82 96,
        Color cor $ rectangleWire 82 96,
        if selecionada then Color cor $ rectangleWire 86 100 else Blank,
        Translate 0 8 $ modeloTorre (TowerRuntime towerId 1 Nothing) 42 selecionada,
        if compravel then Blank else Color (withAlpha 0.45 black) $ rectangleSolid 82 96,
        drawGlossBody (-28) (-33) 0.045 corTextoSuave (take 5 (nomeTowerSpec spec)),
        drawGlossBody 4 (-33) 0.053 corPreco (show preco),
        drawGlossBody (-26) (-45) 0.036 (corRaridade (raridadeTowerSpec spec)) (siglaRaridade (raridadeTowerSpec spec)),
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
vidaMaxInimigoEstimado = vidaMaximaEstimada

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

showTempoCurto :: Float -> String
showTempoCurto valor =
  let arredondado = fromIntegral (round (valor * 10) :: Int) / 10 :: Float
   in show arredondado

showTempoPartida :: Float -> String
showTempoPartida valor =
  let totalSegundos = max 0 (floor valor :: Int)
      minutos = totalSegundos `div` 60
      segundos = totalSegundos `mod` 60
      prefixo = if segundos < 10 then "0" else ""
   in show minutos ++ ":" ++ prefixo ++ show segundos

showDeltaFloat :: Float -> String
showDeltaFloat valor =
  let arredondado = fromIntegral (round (valor * 10) :: Int) / 10 :: Float
   in show arredondado

showDuracaoProjetil :: Projetil -> String
showDuracaoProjetil projetil =
  case duracaoProjetil projetil of
    Infinita -> "INF"
    Finita valor -> showTempoCurto valor ++ "s"

nomeRaridadeUI :: Raridade -> String
nomeRaridadeUI raridade = case raridade of
  Comum -> "COMUM"
  Raro -> "RARO"
  Epico -> "EPICO"
  Lendario -> "LENDARIO"
  Mitico -> "MITICO"

siglaRaridade :: Raridade -> String
siglaRaridade raridade = case raridade of
  Comum -> "C"
  Raro -> "R"
  Epico -> "E"
  Lendario -> "L"
  Mitico -> "M"

corRaridade :: Raridade -> Color
corRaridade raridade = case raridade of
  Comum -> makeColorI 182 194 177 255
  Raro -> makeColorI 144 193 222 255
  Epico -> makeColorI 191 150 232 255
  Lendario -> makeColorI 231 187 113 255
  Mitico -> makeColorI 225 126 158 255

hudPill :: Float -> Float -> Float -> String -> String -> Color -> Picture
hudPill x y w etiqueta valor corValor =
  Translate x y $
    Pictures
      [ Color (withAlpha 0.24 corValor) $ rectangleSolid w 48,
        Color (withAlpha 0.55 corValor) $ rectangleWire w 48,
        drawUITextLeft (-w / 2 + 14) 9 2.2 (makeColorI 176 184 171 255) etiqueta,
        drawUITextLeft (-w / 2 + 72) (-4) 3.1 corValor valor
      ]

mostraVaga :: WaveSummary -> ModoJogoEscolhido -> String
mostraVaga resumo modoAtual
  | modoAtual == ModoInfinito = show (max 1 (ondaAtualUI resumo))
  | otherwise = show (ondaAtualUI resumo) ++ "/" ++ show (ondasTotaisUI resumo)

-- ============================================================================
-- GAME OVER (VITÓRIA/DERROTA)
-- ============================================================================

desenhaResultadoPartida :: ImmutableTowers -> Picture
desenhaResultadoPartida e =
  Pictures
    [ desenhaJogoCompleto (e {modo = EmJogo}),
      Color (withAlpha 0.84 black) $ rectangleSolid larguraJanela alturaJanela,
      case ultimoResumoPartida e of
        Nothing -> desenhaPainelResultadoFallback e
        Just resumo -> desenhaPainelResultado e resumo
    ]

desenhaPainelResultadoFallback :: ImmutableTowers -> Picture
desenhaPainelResultadoFallback e =
  Pictures
    [ drawPanel (UIRect 0 (-18) 600 320),
      drawTituloMenu (-196) 98 "RESULTADO" "",
      drawBodyText (-196) 20 "Resumo da partida indisponivel.",
      drawButton (posicaoRato e) resultMenuRect Danger "MENU"
    ]

desenhaPainelResultado :: ImmutableTowers -> ResumoPartida -> Picture
desenhaPainelResultado e resumo =
  let venceu = resultadoPartidaResumo resumo == PartidaVitoria
      titulo = if venceu then "VITORIA" else "DERROTA"
      subtitulo =
        if venceu
          then "A base resistiu. Podes repetir o nivel ou voltar ao menu."
          else "A base caiu. O resumo fica visivel antes de voltar ao menu."
      corTitulo = if venceu then makeColorI 135 174 111 255 else makeColorI 190 82 72 255
      linhas =
        [ "Modo: " ++ nomeModoJogo (modoPartidaResumo resumo),
          "Mapa: " ++ nomeMapa (mapaPartidaResumo resumo),
          "Tempo: " ++ showTempoPartida (tempoPartidaResumo resumo),
          "Pontuacao: " ++ show (pontuacaoPartidaResumo resumo),
          "Ondas: " ++ show (ondasPartidaResumo resumo),
          "Creditos finais: " ++ show (creditosPartidaResumo resumo),
          "Torres no mapa: " ++ show (torresPartidaResumo resumo)
        ]
   in Pictures
        [ drawPanel (UIRect 0 (-8) 760 430),
          drawTituloMenu (-254) 158 titulo "",
          drawGlossBody (-252) 112 0.088 corTextoSuave subtitulo,
          Color (withAlpha 0.18 corTitulo) $ Translate 0 76 $ rectangleSolid 684 2,
          Pictures
            [ drawBodyText (-252) (68 - fromIntegral i * 42) linha
            | (i, linha) <- zip [0 :: Int ..] linhas
            ],
          if venceu
            then Pictures
              [ drawButton (posicaoRato e) resultMenuRect Danger "MENU",
                drawButton (posicaoRato e) resultReplayRect Primary "REPETIR",
                drawBodyText (-120) (-202) "ESC volta ao menu  |  ENTER repete o nivel"
              ]
            else Pictures
              [ drawButton (posicaoRato e) resultMenuRect Danger "MENU",
                drawBodyText (-132) (-202) "ESC ou ENTER voltam ao menu inicial"
              ]
        ]

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

mapaToPicture :: MapLayoutConfig -> Imagens -> Mapa -> Picture
mapaToPicture cfg _ terreno =
  let bloco = calculaTamanhoBloco cfg terreno
      dims = fromMaybe (MapDimensions 0 0) (dimensoesMapa terreno)
      largura = fromIntegral (larguraMapa dims)
      altura = fromIntegral (alturaMapa dims)
      offsetX = offsetMapaX cfg terreno
      offsetY = offsetMapaY cfg terreno
      margem = 0
   in Pictures [
        Color (makeColorI 38 58 38 255) $
          Translate 0 gameMapCenterY $ rectangleSolid (largura * bloco + margem) (altura * bloco + margem),
        Pictures [
          Translate
            (offsetX + fromIntegral x * bloco + bloco / 2)
            (offsetY + fromIntegral y * bloco + bloco / 2)
            $ blocoToPicture b x y bloco
          | (y, linha) <- zip [0 :: Int ..] (reverse terreno),
            (x, b) <- zip [0 :: Int ..] linha
        ]
      ]

calculaTamanhoBloco :: MapLayoutConfig -> Mapa -> Float
calculaTamanhoBloco cfg terreno =
  fromMaybe 1 (tamanhoBloco cfg terreno)

posicaoMapaParaEcra :: MapLayoutConfig -> Mapa -> Posicao -> (Float, Float)
posicaoMapaParaEcra cfg terreno pos =
  fromMaybe (0, 0) (mapaParaEcra cfg terreno pos)

offsetMapaX :: MapLayoutConfig -> Mapa -> Float
offsetMapaX cfg terreno = maybe 0 fst (offsetMapa cfg terreno)

offsetMapaY :: MapLayoutConfig -> Mapa -> Float
offsetMapaY cfg terreno = maybe 0 snd (offsetMapa cfg terreno)

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
  
  return $
    ImmutableTowers
      { jogo = jogoInicial,
        imagens = imgs,
        modo = MenuInicial Jogar,
        tempo = 0,
        janelaAtual = (round larguraJanela, round alturaJanela),
        torreSelecionada = Nothing,
        torreSelecionadaId = Nothing,
        torreFocada = Nothing,
        registoTorres = emptyTowerRegistry,
        posicaoRato = Nothing,
        perfilJogador = perfilGuardado,
        leaderboardLocal = leaderboardGuardada,
        progressoMeta = metaInicialJogo,
        modoJogoEscolhido = modoGuardado,
        mapaAtual = mapaInicial,
        ondasSobrevividas = 0,
        totalOndasPartida = totalOndas,
        resultadoRegistado = False,
        velocidadeJogo = 1,
        mensagensUI = [],
        hudCompacto = False,
        lojaVisivel = True,
        efeitosUpgrade = [],
        ultimoResumoPartida = Nothing,
        botAutomatico = False,
        botCooldown = 0,
        backspacePerfilAtivo = False,
        backspacePerfilTimer = 0
      }

ratoParaCelula :: MapLayoutConfig -> Mapa -> (Float, Float) -> Maybe (Int, Int, Float, Float)
ratoParaCelula cfg mapa (mx, my) =
  case ecraParaCelula cfg mapa (mx, my) of
    Just (cx, cy) -> do
      (centroX, centroY) <- mapaParaEcra cfg mapa (fromIntegral cx + 0.5, fromIntegral cy + 0.5)
      return (cx, cy, centroX, centroY)
    Nothing -> Nothing

terrenoEm :: Int -> Int -> Mapa -> Maybe Terreno
terrenoEm x y mapa = terrenoEmCelula mapa x y

caminhoImagem :: FilePath -> FilePath
caminhoImagem nome = "app/imagens/" ++ nome
