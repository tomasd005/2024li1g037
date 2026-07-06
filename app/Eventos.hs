module Eventos where

import Data.List
import Graphics.Gloss.Interface.IO.Game
import ImmutableTowers
import LI12425
import Tarefa1
import System.Exit (exitSuccess)

mapaLarguraMaxInput, mapaAlturaMaxInput, mapaCentroYInput :: Float
mapaLarguraMaxInput = 900
mapaAlturaMaxInput = 760
mapaCentroYInput = 20

reage :: Event -> ImmutableTowers -> IO ImmutableTowers

-- ============================================================================
-- MENU PRINCIPAL - Navegação com setas
-- ============================================================================

reage (EventMotion pos) estado = return estado {posicaoRato = Just pos}

reage (EventKey (SpecialKey KeyRight) Down _ _) e@(ImmutableTowers _ _ modo _ _ _) =
  case modo of
    MenuInicial Jogar -> return e {modo = MenuInicial Creditos}
    MenuInicial Creditos -> return e {modo = MenuInicial Sair}
    _ -> return e

reage (EventKey (SpecialKey KeyLeft) Down _ _) e@(ImmutableTowers _ _ modo _ _ _) =
  case modo of
    MenuInicial Creditos -> return e {modo = MenuInicial Jogar}
    MenuInicial Sair -> return e {modo = MenuInicial Creditos}
    _ -> return e

-- ============================================================================
-- MENU PRINCIPAL - Navegação com cliques
-- ============================================================================

reage (EventKey (MouseButton LeftButton) Down _ (mx, my)) e@(ImmutableTowers _ _ (MenuInicial _) _ _ _) = do
  -- Botão Jogar (esquerda)
  if mx >= -420 && mx <= -180 && my >= -179 && my <= -61
    then return e {modo = EmJogo}
  -- Botão Créditos/Tutorial (centro)
  else if mx >= -120 && mx <= 120 && my >= -179 && my <= -61
    then return e {modo = TutorialFoto}
  -- Botão Sair (direita)
  else if mx >= 180 && mx <= 420 && my >= -179 && my <= -61
    then exitSuccess
  else return e

-- ============================================================================
-- ENTER para confirmar seleção no menu
-- ============================================================================

reage (EventKey (SpecialKey KeyEnter) Down _ _) e@(ImmutableTowers _ _ modo _ _ _) =
  case modo of
    MenuInicial Jogar -> return e {modo = EmJogo}
    MenuInicial Sair -> exitSuccess
    MenuInicial Creditos -> return e {modo = TutorialFoto}
    TutorialFoto -> return e {modo = MenuInicial Creditos}
    MostrarCreditos -> return e {modo = MenuInicial Creditos}
    _ -> return e

-- ============================================================================
-- ESC para sair do tutorial ou do jogo
-- ============================================================================

reage (EventKey (SpecialKey KeyEsc) Down _ _) e@(ImmutableTowers _ _ modo _ _ _) =
  case modo of
    TutorialFoto -> return e {modo = MenuInicial Creditos}
    EmJogo -> 
      let jogo' = jogo e
          base = baseJogo jogo'
          inimigos = inimigosJogo jogo'
          ondas = concatMap ondasPortal (portaisJogo jogo')
          ganhou = vidaBase base > 0 && null inimigos && all (null . inimigosOnda) ondas
          perdeu = vidaBase base <= 0
       in if ganhou || perdeu
          then exitSuccess  -- Sai do jogo se ganhou ou perdeu
          else return e     -- Senão continua jogando
    _ -> return e

-- ============================================================================
-- PAUSAR e RETOMAR jogo
-- ============================================================================

reage (EventKey (Char 'p') Down _ _) e@(ImmutableTowers _ _ modo _ _ _) =
  case modo of
    EmJogo -> return e {modo = Pausado}
    Pausado -> return e {modo = EmJogo}
    _ -> return e

-- ============================================================================
-- CANCELAR seleção de torre com botão direito
-- ============================================================================

reage (EventKey (MouseButton RightButton) Down _ _) estado@(ImmutableTowers _ _ EmJogo _ (Just _) _) =
  return estado {torreSelecionada = Nothing}

-- ============================================================================
-- SELECIONAR torre na loja (sem torre selecionada)
-- ============================================================================

reage (EventKey (MouseButton LeftButton) Down _ (mx, my)) estado@(ImmutableTowers jogo _ EmJogo _ Nothing _) = do
  let itensLoja = zip [0 ..] (lojaJogo jogo)
      itemClicado = find (\(i, _) -> lojaClicada mx my i) itensLoja
  
  case itemClicado of
    Just (_, (preco, torre)) -> do
      if creditosBase (baseJogo jogo) >= preco
        then return estado {torreSelecionada = Just torre, posicaoRato = Just (mx, my)}
        else return estado
    Nothing -> return estado {posicaoRato = Just (mx, my)}

-- ============================================================================
-- COLOCAR torre no mapa (com torre selecionada)
-- ============================================================================

reage (EventKey (MouseButton LeftButton) Down _ (mx, my)) estado@(ImmutableTowers jogo _ EmJogo _ (Just torre) _) = do
  let mapa = mapaJogo jogo
      largura = fromIntegral (length (head mapa))
      altura = fromIntegral (length mapa)
      bloco = min (mapaLarguraMaxInput / largura) (mapaAlturaMaxInput / altura)
      offsetX = -(largura * bloco) / 2
      offsetY = mapaCentroYInput - (altura * bloco) / 2
  
  -- Converter coordenadas de tela para coordenadas do mapa
  let posX = floor ((mx - offsetX) / bloco) :: Int
      posY = floor (altura - ((my - offsetY) / bloco)) :: Int
      novaPosicao = (fromIntegral posX + 0.5, fromIntegral posY + 0.5)
      
  -- Verificar se a posição é válida
  let terrenoValido = terrenoPorPosicao (fromIntegral posX, fromIntegral posY) mapa == Just Relva
      posicaoLivre = notElem novaPosicao (map posicaoTorre (torresJogo jogo))
  
  if terrenoValido && posicaoLivre
    then do
      -- Encontrar o preço da torre na loja
      let precoTorre = case find (\(_, t) -> mesmoTipoTorre t torre) (lojaJogo jogo) of
                         Just (preco, _) -> preco
                         Nothing -> 0
      
      let novaTorre = torre {posicaoTorre = novaPosicao}
          novasTorres = novaTorre : torresJogo jogo
          novosCreditos = creditosBase (baseJogo jogo) - precoTorre
          novaBase = (baseJogo jogo) {creditosBase = novosCreditos}
      
      return estado {
        jogo = jogo {
          torresJogo = novasTorres,
          baseJogo = novaBase
        },
        torreSelecionada = Nothing,
        posicaoRato = Just (mx, my)
      }
    else return estado {posicaoRato = Just (mx, my)}

-- ============================================================================
-- CASO PADRÃO
-- ============================================================================

reage _ estado = return estado

-- ============================================================================
-- FUNÇÕES AUXILIARES
-- ============================================================================

-- | Verifica se duas torres são do mesmo tipo
mesmoTipoTorre :: Torre -> Torre -> Bool
mesmoTipoTorre t1 t2 = tipoProjetil (projetilTorre t1) == tipoProjetil (projetilTorre t2)

-- | Função auxiliar para detectar cliques na loja (CORRIGIDA)
lojaClicada :: Float -> Float -> Int -> Bool
lojaClicada mx my indice =
  let posX = -larguraJanela/2 + 118 + fromIntegral indice * 136
      posY = -alturaJanela/2 + 82
      largura = 116
      altura = 116
   in mx >= (posX - largura/2) && 
      mx <= (posX + largura/2) && 
      my >= (posY - altura/2) && 
      my <= (posY + altura/2)