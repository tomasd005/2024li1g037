module Eventos where

import Data.List
import GHC.Float (int2Float)
import Graphics.Gloss.Interface.IO.Game
import ImmutableTowers
import LI12425
import Tarefa1
import System.Exit (exitSuccess)

reage :: Event -> ImmutableTowers -> IO ImmutableTowers

-- ============================================================================
-- MENU PRINCIPAL - Navegação com setas
-- ============================================================================

reage (EventKey (SpecialKey KeyRight) Down _ _) e@(ImmutableTowers _ _ modo _ _) =
  case modo of
    MenuInicial Jogar -> return e {modo = MenuInicial Creditos}
    MenuInicial Creditos -> return e {modo = MenuInicial Sair}
    _ -> return e

reage (EventKey (SpecialKey KeyLeft) Down _ _) e@(ImmutableTowers _ _ modo _ _) =
  case modo of
    MenuInicial Creditos -> return e {modo = MenuInicial Jogar}
    MenuInicial Sair -> return e {modo = MenuInicial Creditos}
    _ -> return e

-- ============================================================================
-- MENU PRINCIPAL - Navegação com cliques
-- ============================================================================

reage (EventKey (MouseButton LeftButton) Down _ (mx, my)) e@(ImmutableTowers _ _ (MenuInicial _) _ _) = do
  -- Botão Jogar (esquerda)
  if mx >= -500 && mx <= -300 && my >= -900 && my <= -750
    then return e {modo = EmJogo}
  -- Botão Créditos (centro)
  else if mx >= -100 && mx <= 100 && my >= -900 && my <= -750
    then return e {modo = TutorialFoto}
  -- Botão Sair (direita)
  else if mx >= 300 && mx <= 500 && my >= -900 && my <= -750
    then exitSuccess
  else return e

-- ============================================================================
-- ENTER para confirmar seleção no menu
-- ============================================================================

reage (EventKey (SpecialKey KeyEnter) Down _ _) e@(ImmutableTowers _ _ modo _ _) =
  case modo of
    MenuInicial Jogar -> return e {modo = EmJogo}
    MenuInicial Sair -> exitSuccess
    MenuInicial Creditos -> return e {modo = TutorialFoto}
    MostrarCreditos -> return e {modo = MenuInicial Creditos}
    _ -> return e

-- ============================================================================
-- ESC para sair do tutorial ou do jogo
-- ============================================================================

reage (EventKey (SpecialKey KeyEsc) Down _ _) e@(ImmutableTowers _ _ modo _ _) =
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

reage (EventKey (Char 'p') Down _ _) e@(ImmutableTowers _ _ modo _ _) =
  case modo of
    EmJogo -> return e {modo = Pausado}
    Pausado -> return e {modo = EmJogo}
    _ -> return e

-- ============================================================================
-- CANCELAR seleção de torre com botão direito
-- ============================================================================

reage (EventKey (MouseButton RightButton) Down _ _) estado@(ImmutableTowers _ _ EmJogo _ (Just _)) = do
  putStrLn "Torre desselecionada"
  return estado {torreSelecionada = Nothing}

-- ============================================================================
-- SELECIONAR torre na loja (sem torre selecionada)
-- ============================================================================

reage (EventKey (MouseButton LeftButton) Down _ (mx, my)) estado@(ImmutableTowers jogo _ EmJogo _ Nothing) = do
  let itensLoja = zip [0 ..] (lojaJogo jogo)
      itemClicado = find (\(i, _) -> lojaClicada mx my i) itensLoja
  
  putStrLn $ "Clique em: (" ++ show mx ++ ", " ++ show my ++ ")"
  
  case itemClicado of
    Just (indice, (preco, torre)) -> do
      putStrLn $ "Torre selecionada: índice " ++ show indice ++ ", preço " ++ show preco
      if creditosBase (baseJogo jogo) >= preco
        then do
          putStrLn "Créditos suficientes! Torre selecionada."
          return estado {torreSelecionada = Just torre}
        else do
          putStrLn "Créditos insuficientes!"
          return estado
    Nothing -> do
      putStrLn "Clique fora da loja"
      return estado

-- ============================================================================
-- COLOCAR torre no mapa (com torre selecionada)
-- ============================================================================

reage (EventKey (MouseButton LeftButton) Down _ (mx, my)) estado@(ImmutableTowers jogo _ EmJogo _ (Just torre)) = do
  let mapa = mapaJogo jogo
      largura = fromIntegral (length (head mapa))
      altura = fromIntegral (length mapa)
      bloco = min (larguraJanela / largura) (alturaJanela / altura)
  
  -- Converter coordenadas de tela para coordenadas do mapa
  let posX = floor $ (mx + (larguraJanela / 2)) / bloco
      posY = floor $ ((alturaJanela / 2) - my) / bloco
      novaPosicao = (fromIntegral posX + 0.5, fromIntegral posY + 0.5)
      
  putStrLn $ "Tentando colocar torre em: " ++ show novaPosicao
  putStrLn $ "Coordenadas de tela: (" ++ show mx ++ ", " ++ show my ++ ")"
  putStrLn $ "Coordenadas do mapa: (" ++ show posX ++ ", " ++ show posY ++ ")"
  
  -- Verificar se a posição é válida
  let terrenoValido = terrenoPorPosicao (fromIntegral posX, fromIntegral posY) mapa == Just Relva
      posicaoLivre = notElem novaPosicao (map posicaoTorre (torresJogo jogo))
      
  putStrLn $ "Terreno válido (Relva): " ++ show terrenoValido
  putStrLn $ "Posição livre: " ++ show posicaoLivre
  
  if terrenoValido && posicaoLivre
    then do
      -- Encontrar o preço da torre na loja
      let precoTorre = case find (\(_, t) -> mesmoTipoTorre t torre) (lojaJogo jogo) of
                         Just (preco, _) -> preco
                         Nothing -> 0
      
      putStrLn $ "Torre colocada! Preço: " ++ show precoTorre
      
      let novaTorre = torre {posicaoTorre = novaPosicao}
          novasTorres = novaTorre : torresJogo jogo
          novosCreditos = creditosBase (baseJogo jogo) - precoTorre
          novaBase = (baseJogo jogo) {creditosBase = novosCreditos}
      
      return estado {
        jogo = jogo {
          torresJogo = novasTorres,
          baseJogo = novaBase
        },
        torreSelecionada = Nothing
      }
    else do
      putStrLn "Posição inválida para torre."
      return estado

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
  let posX = -larguraJanela/2 + 150 + fromIntegral indice * 160
      posY = -alturaJanela/2 + 100
      largura = 140
      altura = 140
   in mx >= (posX - largura/2) && 
      mx <= (posX + largura/2) && 
      my >= (posY - altura/2) && 
      my <= (posY + altura/2)