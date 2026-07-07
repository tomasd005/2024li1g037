module Eventos where

import BotSystem
import Data.List
import GameFactory
import Graphics.Gloss.Interface.IO.Game
import ImmutableTowers
import LI12425
import qualified MapEditor as Editor
import MapGeometry
import SaveSystem
import System.Exit (exitSuccess)
import TowerSystem
import UIComponents

mapaLarguraMaxInput, mapaAlturaMaxInput, mapaCentroYInput :: Float
mapaLarguraMaxInput = 900
mapaAlturaMaxInput = 840
mapaCentroYInput = 8

editorConfig :: MapLayoutConfig
editorConfig = MapLayoutConfig mapaLarguraMaxInput mapaAlturaMaxInput mapaCentroYInput

reage :: Event -> ImmutableTowers -> IO ImmutableTowers
reage (EventMotion pos) estado = return estado {posicaoRato = Just pos}
reage (EventKey (SpecialKey KeyRight) Down _ _) e =
  case modo e of
    MenuInicial Jogar -> return e {modo = MenuInicial Modos}
    MenuInicial Modos -> return e {modo = MenuInicial Perfil}
    MenuInicial Perfil -> return e {modo = MenuInicial Leaderboard}
    MenuInicial Leaderboard -> return e {modo = MenuInicial Creditos}
    MenuInicial Creditos -> return e {modo = MenuInicial Opcoes}
    MenuInicial Opcoes -> return e {modo = MenuInicial Sair}
    SelecionarModo -> guardarModoSelecionado e (proximoModoJogo (modoJogoEscolhido e))
    _ -> return e
reage (EventKey (SpecialKey KeyLeft) Down _ _) e =
  case modo e of
    MenuInicial Modos -> return e {modo = MenuInicial Jogar}
    MenuInicial Perfil -> return e {modo = MenuInicial Modos}
    MenuInicial Leaderboard -> return e {modo = MenuInicial Perfil}
    MenuInicial Creditos -> return e {modo = MenuInicial Leaderboard}
    MenuInicial Opcoes -> return e {modo = MenuInicial Creditos}
    MenuInicial Sair -> return e {modo = MenuInicial Opcoes}
    SelecionarModo -> guardarModoSelecionado e (modoJogoAnterior (modoJogoEscolhido e))
    _ -> return e
reage (EventKey (MouseButton LeftButton) Down _ (mx, my)) e@(ImmutableTowers _ _ (MenuInicial _) _ _ _ _ _ _ _ _ _ _ _) =
  case opcaoMenuClicada mx my of
    Just Jogar -> return (iniciarPartida e)
    Just Modos -> return e {modo = SelecionarModo}
    Just Perfil -> return e {modo = MostrarPerfil}
    Just Leaderboard -> return e {modo = MostrarLeaderboard}
    Just Creditos -> return e {modo = TutorialFoto}
    Just Opcoes -> return e {modo = MostrarOpcoes}
    Just Sair -> exitSuccess
    Nothing -> return e
reage (EventKey (MouseButton LeftButton) Down _ (mx, my)) e@(ImmutableTowers _ _ SelecionarModo _ _ _ _ _ _ _ _ _ _ _) =
  case modoClicado mx my of
    Just modoEscolhido -> guardarModoSelecionado e modoEscolhido
    Nothing -> return e
reage (EventKey (SpecialKey KeyEnter) Down _ _) e =
  case modo e of
    MenuInicial Jogar -> return (iniciarPartida e)
    MenuInicial Modos -> return e {modo = SelecionarModo}
    MenuInicial Perfil -> return e {modo = MostrarPerfil}
    MenuInicial Leaderboard -> return e {modo = MostrarLeaderboard}
    MenuInicial Sair -> exitSuccess
    MenuInicial Creditos -> return e {modo = TutorialFoto}
    MenuInicial Opcoes -> return e {modo = MostrarOpcoes}
    TutorialFoto -> return e {modo = MenuInicial Creditos}
    MostrarCreditos -> return e {modo = MenuInicial Creditos}
    MostrarPerfil -> return e {modo = MenuInicial Perfil}
    MostrarLeaderboard -> return e {modo = MenuInicial Leaderboard}
    MostrarOpcoes -> return e {modo = MenuInicial Opcoes}
    SelecionarModo -> return e {modo = MenuInicial Modos}
    EditorMapa -> return e {modo = MenuInicial Modos}
    _ -> return e
reage (EventKey (Char 'e') Down _ _) e =
  case modo e of
    MenuInicial _ -> return e {modo = EditorMapa}
    _ -> return e
reage (EventKey (SpecialKey KeyBackspace) Down _ _) estado = apagarCaracterPerfil estado
reage (EventKey (SpecialKey KeyDelete) Down _ _) estado = apagarCaracterPerfil estado
reage (EventKey (Char '\b') Down _ _) estado = apagarCaracterPerfil estado
reage (EventKey (Char '\DEL') Down _ _) estado = apagarCaracterPerfil estado
reage (EventKey (Char c) Down _ _) estado@(ImmutableTowers _ _ MostrarPerfil _ _ _ _ perfil leaderboard modoAtual _ _ _ _)
  | c >= ' ' && c <= '~' && length (nomeJogador perfil) < 16 =
      let novoPerfil = perfil {nomeJogador = nomeJogador perfil ++ [c]}
       in do
            guardarMetaEstado novoPerfil leaderboard modoAtual
            return estado {perfilJogador = novoPerfil}
  | otherwise = return estado
reage (EventKey (SpecialKey KeyEsc) Down _ _) e =
  case modo e of
    TutorialFoto -> return e {modo = MenuInicial Creditos}
    MostrarPerfil -> return e {modo = MenuInicial Perfil}
    MostrarLeaderboard -> return e {modo = MenuInicial Leaderboard}
    MostrarOpcoes -> return e {modo = MenuInicial Opcoes}
    SelecionarModo -> return e {modo = MenuInicial Modos}
    EditorMapa -> return e {modo = MenuInicial Modos}
    EmJogo ->
      let jogoAtual = jogo e
          base = baseJogo jogoAtual
          inimigos = inimigosJogo jogoAtual
          ondas = concatMap ondasPortal (portaisJogo jogoAtual)
          terminou = vidaBase base <= 0 || (null inimigos && all (null . inimigosOnda) ondas)
       in if terminou then exitSuccess else return e
    _ -> return e
reage (EventKey (Char 'p') Down _ _) e =
  case modo e of
    EmJogo -> return e {modo = Pausado}
    Pausado -> return e {modo = EmJogo}
    _ -> return e
reage (EventKey (Char 'x') Down _ _) e =
  case modo e of
    EmJogo ->
      let novaVel = proximaVelocidade (velocidadeJogo e)
       in return $ adicionaMensagem MsgInfo ("Velocidade " ++ show (round novaVel :: Int) ++ "x") e {velocidadeJogo = novaVel}
    _ -> return e
reage (EventKey (Char 'u') Down _ _) estado@(ImmutableTowers jogoAtual _ EmJogo _ _ foco _ _ _ _ _ _ _ _) =
  return $ upgradeSelecionada estado jogoAtual foco
reage (EventKey (Char 's') Down _ _) estado@(ImmutableTowers jogoAtual _ EmJogo _ _ _ _ _ _ _ _ _ _ _) = do
  guardarJogoLocal jogoAtual
  return $ adicionaMensagem MsgSucesso "Jogo guardado" estado
reage (EventKey (Char 'l') Down _ _) estado = do
  guardado <- carregarJogoLocal
  case guardado of
    Just jogoGuardado -> return estado {jogo = jogoGuardado, modo = EmJogo, torreSelecionada = Nothing, torreFocada = Nothing}
    Nothing -> return estado
reage (EventKey (Char 'b') Down _ _) estado@(ImmutableTowers jogoAtual _ EmJogo _ _ _ _ _ _ _ _ _ _ _) =
  return $ adicionaMensagem MsgInfo "Bot sugeriu uma construcao" estado {jogo = botColocaTorre jogoAtual}
reage (EventKey (Char 'o') Down _ _) estado@(ImmutableTowers jogoAtual _ EmJogo _ _ _ rato _ _ _ _ _ _ _) =
  return $ case rato of
    Just pos -> adicionaMensagem MsgAviso "Obstaculo colocado" estado {jogo = jogoAtual {mapaJogo = Editor.colocaObstaculo editorConfig pos (mapaJogo jogoAtual)}}
    Nothing -> estado
reage (EventKey (MouseButton LeftButton) Down _ pos) estado@(ImmutableTowers jogoAtual _ EditorMapa _ _ _ _ _ _ _ _ _ _ _) =
  return estado {jogo = jogoAtual {mapaJogo = Editor.cicloCelulaMapa editorConfig pos (mapaJogo jogoAtual)}}
reage (EventKey (MouseButton RightButton) Down _ _) estado@(ImmutableTowers _ _ EmJogo _ _ _ _ _ _ _ _ _ _ _) =
  return estado {torreSelecionada = Nothing, torreFocada = Nothing}
reage (EventKey (MouseButton LeftButton) Down _ pos@(mx, my)) estado@(ImmutableTowers jogoAtual _ EmJogo _ Nothing _ _ _ _ _ _ _ _ _) =
  case acaoJogoClicada pos estado of
    Just novoEstado -> return novoEstado
    Nothing ->
      case find (\(i, _) -> lojaClicada mx my i) (zip [0 ..] (lojaJogo jogoAtual)) of
        Just (_, (preco, torre))
          | creditosBase (baseJogo jogoAtual) >= preco ->
              return $ adicionaMensagem MsgInfo ("Selecionada " ++ nomeTipoProjetil (projetilTorre torre)) estado {torreSelecionada = Just torre, torreFocada = Nothing, posicaoRato = Just (mx, my)}
        _ -> case torreNoClique (mx, my) jogoAtual of
          Just torre -> return estado {torreFocada = Just (posicaoTorre torre), posicaoRato = Just (mx, my)}
          Nothing -> return estado {torreFocada = Nothing, posicaoRato = Just (mx, my)}
reage (EventKey (MouseButton LeftButton) Down _ (mx, my)) estado@(ImmutableTowers jogoAtual _ EmJogo _ (Just torre) _ _ _ _ _ _ _ _ _) =
  case acaoJogoClicada (mx, my) estado of
    Just novoEstado -> return novoEstado
    Nothing -> return $ colocaTorreSelecionada estado jogoAtual torre (mx, my)
reage (EventKey (MouseButton LeftButton) Down _ pos) estado@ImmutableTowers {modo = Pausado} =
  return $ case acaoPausaClicada pos estado of
    Just novoEstado -> novoEstado
    Nothing -> estado
reage _ estado = return estado

apagarCaracterPerfil :: ImmutableTowers -> IO ImmutableTowers
apagarCaracterPerfil estado =
  case modo estado of
    MostrarPerfil ->
      let perfil = perfilJogador estado
          nomeAtual = nomeJogador perfil
          novoPerfil = perfil {nomeJogador = if null nomeAtual then nomeAtual else init nomeAtual}
       in do
            guardarMetaEstado novoPerfil (leaderboardLocal estado) (modoJogoEscolhido estado)
            return estado {perfilJogador = novoPerfil}
    _ -> return estado

acaoJogoClicada :: (Float, Float) -> ImmutableTowers -> Maybe ImmutableTowers
acaoJogoClicada pos estado
  | pos `containsPoint` startWaveRect = Just (adicionaMensagem MsgInfo "Vaga iniciada" (iniciaProximaVaga estado))
  | pos `containsPoint` pauseRect = Just estado {modo = Pausado}
  | pos `containsPoint` speed1Rect = Just (adicionaMensagem MsgInfo "Velocidade 1x" estado {velocidadeJogo = 1})
  | pos `containsPoint` speed2Rect = Just (adicionaMensagem MsgInfo "Velocidade 2x" estado {velocidadeJogo = 2})
  | pos `containsPoint` speed4Rect = Just (adicionaMensagem MsgInfo "Velocidade 4x" estado {velocidadeJogo = 4})
  | pos `containsPoint` upgradeRect = Just (upgradeSelecionada estado (jogo estado) (torreFocada estado))
  | pos `containsPoint` sellRect = Just (vendeTorreSelecionada estado)
  | pos `containsPoint` cancelRect = Just estado {torreSelecionada = Nothing, torreFocada = Nothing}
  | otherwise = Nothing

acaoPausaClicada :: (Float, Float) -> ImmutableTowers -> Maybe ImmutableTowers
acaoPausaClicada pos estado
  | pos `containsPoint` resumeRect = Just (adicionaMensagem MsgInfo "Continuar" estado {modo = EmJogo})
  | pos `containsPoint` restartRect = Just (adicionaMensagem MsgInfo "Partida reiniciada" (iniciarPartida estado))
  | pos `containsPoint` menuRect = Just estado {modo = MenuInicial Jogar, torreSelecionada = Nothing, torreFocada = Nothing}
  | otherwise = Nothing

proximaVelocidade :: Float -> Float
proximaVelocidade velocidade
  | velocidade < 2 = 2
  | velocidade < 4 = 4
  | otherwise = 1

iniciaProximaVaga :: ImmutableTowers -> ImmutableTowers
iniciaProximaVaga estado =
  let jogoAtual = jogo estado
      portaisAtualizados = map iniciaPortal (portaisJogo jogoAtual)
   in estado {jogo = jogoAtual {portaisJogo = portaisAtualizados}}
  where
    iniciaPortal portal =
      case ondasPortal portal of
        [] -> portal
        onda : ondas -> portal {ondasPortal = onda {entradaOnda = 0, tempoOnda = 0} : ondas}

vendeTorreSelecionada :: ImmutableTowers -> ImmutableTowers
vendeTorreSelecionada estado =
  case torreFocada estado of
    Nothing -> adicionaMensagem MsgAviso "Seleciona uma torre primeiro" estado
    Just pos ->
      let jogoAtual = jogo estado
          (vendidas, restantes) = partition ((== pos) . posicaoTorre) (torresJogo jogoAtual)
          refund = sum (map valorVendaTorre vendidas)
          base = baseJogo jogoAtual
          baseAtualizada = base {creditosBase = creditosBase base + refund}
       in if null vendidas
          then adicionaMensagem MsgAviso "Nenhuma torre selecionada" estado
          else adicionaMensagem MsgSucesso ("Torre vendida +" ++ show refund) estado {jogo = jogoAtual {torresJogo = restantes, baseJogo = baseAtualizada}, torreFocada = Nothing}

adicionaMensagem :: TipoMensagem -> String -> ImmutableTowers -> ImmutableTowers
adicionaMensagem tipo texto estado =
  estado {mensagensUI = MensagemUI texto 2.4 tipo : take 3 (mensagensUI estado)}

guardarModoSelecionado :: ImmutableTowers -> ModoJogoEscolhido -> IO ImmutableTowers
guardarModoSelecionado e novoModo = do
  guardarMetaEstado (perfilJogador e) (leaderboardLocal e) novoModo
  return e {modoJogoEscolhido = novoModo}

iniciarPartida :: ImmutableTowers -> ImmutableTowers
iniciarPartida e =
  let jogoNovo = jogoParaModo (modoJogoEscolhido e)
   in e
        { jogo = jogoNovo,
          modo = EmJogo,
          tempo = 0,
          torreSelecionada = Nothing,
          torreFocada = Nothing,
          ondasSobrevividas = 0,
          resultadoRegistado = False,
          velocidadeJogo = 1,
          mensagensUI = []
        }

upgradeSelecionada :: ImmutableTowers -> Jogo -> Maybe Posicao -> ImmutableTowers
upgradeSelecionada estado jogoAtual foco =
  case foco >>= \pos -> findIndex (\t -> posicaoTorre t == pos) (torresJogo jogoAtual) of
    Nothing -> adicionaMensagem MsgAviso "Seleciona uma torre para melhorar" estado
    Just indice ->
      let torre = torresJogo jogoAtual !! indice
          custo = custoUpgradeTorre torre
          base = baseJogo jogoAtual
       in if creditosBase base < custo
          then adicionaMensagem MsgErro "Creditos insuficientes" estado
          else
            let torresAtualizadas = atualizaIndice indice upgradeTorre (torresJogo jogoAtual)
                baseAtualizada = base {creditosBase = creditosBase base - custo}
             in adicionaMensagem MsgSucesso "Torre melhorada" estado {jogo = jogoAtual {torresJogo = torresAtualizadas, baseJogo = baseAtualizada}}

colocaTorreSelecionada :: ImmutableTowers -> Jogo -> Torre -> (Float, Float) -> ImmutableTowers
colocaTorreSelecionada estado jogoAtual torre (mx, my) =
  let mapa = mapaJogo jogoAtual
      celula = ecraParaCelula editorConfig mapa (mx, my)
      (posX, posY) = maybe (-1, -1) id celula
      novaPosicao = (fromIntegral posX + 0.5, fromIntegral posY + 0.5)
      terrenoValido = terrenoEmCelula mapa posX posY == Just Relva
      posicaoLivre = notElem novaPosicao (map posicaoTorre (torresJogo jogoAtual))
   in if terrenoValido && posicaoLivre
      then
        let precoTorre = maybe 0 fst (find (\(_, t) -> mesmoModeloTorre t torre) (lojaJogo jogoAtual))
            novaBase = (baseJogo jogoAtual) {creditosBase = creditosBase (baseJogo jogoAtual) - precoTorre}
            novasTorres = torre {posicaoTorre = novaPosicao} : torresJogo jogoAtual
         in estado {jogo = jogoAtual {torresJogo = novasTorres, baseJogo = novaBase}, torreSelecionada = Nothing, torreFocada = Just novaPosicao, posicaoRato = Just (mx, my)}
          |> adicionaMensagem MsgSucesso "Torre construida"
      else adicionaMensagem MsgErro "Nao podes construir aqui" estado {posicaoRato = Just (mx, my)}

(|>) :: a -> (a -> b) -> b
(|>) valor f = f valor

nomeTipoProjetil :: Projetil -> String
nomeTipoProjetil projetil = case tipoProjetil projetil of
  Resina -> "Resina"
  Gelo -> "Gelo"
  Fogo -> "Fogo"
  Medo -> "Medo"
  Veneno -> "Veneno"
  Eletrico -> "Eletrica"

torreNoClique :: (Float, Float) -> Jogo -> Maybe Torre
torreNoClique (mx, my) jogoAtual =
  find dentro (torresJogo jogoAtual)
  where
    dentro torre =
      case (mapaParaEcra editorConfig (mapaJogo jogoAtual) (posicaoTorre torre), tamanhoBloco editorConfig (mapaJogo jogoAtual)) of
        (Just (sx, sy), Just bloco) -> abs (mx - sx) <= bloco * 0.45 && abs (my - sy) <= bloco * 0.45
        _ -> False

atualizaIndice :: Int -> (a -> a) -> [a] -> [a]
atualizaIndice _ _ [] = []
atualizaIndice 0 f (x : xs) = f x : xs
atualizaIndice n f (x : xs) = x : atualizaIndice (n - 1) f xs

lojaClicada :: Float -> Float -> Int -> Bool
lojaClicada mx my indice =
  let posX = -larguraJanela / 2 + 74 + fromIntegral indice * 82
      posY = -alturaJanela / 2 + 72
      largura = 72
      altura = 104
   in mx >= posX - largura / 2
        && mx <= posX + largura / 2
        && my >= posY - altura / 2
        && my <= posY + altura / 2

opcaoMenuClicada :: Float -> Float -> Maybe MenuInicialOpcoes
opcaoMenuClicada mx my = fmap fst $ find (dentroBotao . snd) botoes
  where
    botoes =
      [ (Jogar, (-280, 55)),
        (Modos, (0, 55)),
        (Perfil, (280, 55)),
        (Leaderboard, (-280, -105)),
        (Creditos, (0, -105)),
        (Opcoes, (280, -105)),
        (Sair, (0, -255))
      ]
    dentroBotao (x, y) =
      mx >= x - 120 && mx <= x + 120 && my >= y - 59 && my <= y + 59

modoClicado :: Float -> Float -> Maybe ModoJogoEscolhido
modoClicado mx my = fmap fst $ find (dentro . snd) botoes
  where
    botoes =
      [ (ModoHistoria, (-360, 90)),
        (ModoInfinito, (0, 90)),
        (ModoDesafio, (360, 90)),
        (ModoBoss, (-180, -105)),
        (ModoSandbox, (180, -105))
      ]
    dentro (x, y) =
      mx >= x - 150 && mx <= x + 150 && my >= y - 76 && my <= y + 76

proximoModoJogo :: ModoJogoEscolhido -> ModoJogoEscolhido
proximoModoJogo modoAtual = case modoAtual of
  ModoHistoria -> ModoInfinito
  ModoInfinito -> ModoDesafio
  ModoDesafio -> ModoBoss
  ModoBoss -> ModoSandbox
  ModoSandbox -> ModoHistoria

modoJogoAnterior :: ModoJogoEscolhido -> ModoJogoEscolhido
modoJogoAnterior modoAtual = case modoAtual of
  ModoHistoria -> ModoSandbox
  ModoInfinito -> ModoHistoria
  ModoDesafio -> ModoInfinito
  ModoBoss -> ModoDesafio
  ModoSandbox -> ModoBoss

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
