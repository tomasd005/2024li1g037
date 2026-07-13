module Eventos where

import BotSystem
import Data.List
import Data.Maybe (fromMaybe)
import GameFactory
import Graphics.Gloss.Interface.IO.Game
import ImmutableTowers
import LI12425
import qualified MapEditor as Editor
import MapGeometry
import MetaTypes
import RenderConfig
import SaveSystem
import System.Exit (exitSuccess)
import TowerSystem
import TowerRuntime
import UIComponents
import UIRects

reage :: Event -> ImmutableTowers -> IO ImmutableTowers
reage (EventMotion pos) estado = return estado {posicaoRato = Just (normalizaUIPos estado pos)}
reage (EventResize (w, h)) estado = return estado {janelaAtual = (w, h)}
reage (EventKey (SpecialKey KeyRight) Down _ _) e =
  case modo e of
    MenuInicial Jogar -> return e {modo = MenuInicial Modos}
    MenuInicial Modos -> return e {modo = MenuInicial LojaMeta}
    MenuInicial LojaMeta -> return e {modo = MenuInicial Perfil}
    MenuInicial Perfil -> return e {modo = MenuInicial Leaderboard}
    MenuInicial Leaderboard -> return e {modo = MenuInicial Creditos}
    MenuInicial Creditos -> return e {modo = MenuInicial Opcoes}
    MenuInicial Opcoes -> return e {modo = MenuInicial Sair}
    SelecionarModo -> guardarModoSelecionado e (proximoModoJogo (modoJogoEscolhido e))
    _ -> return e
reage (EventKey (SpecialKey KeyLeft) Down _ _) e =
  case modo e of
    MenuInicial Modos -> return e {modo = MenuInicial Jogar}
    MenuInicial LojaMeta -> return e {modo = MenuInicial Modos}
    MenuInicial Perfil -> return e {modo = MenuInicial LojaMeta}
    MenuInicial Leaderboard -> return e {modo = MenuInicial Perfil}
    MenuInicial Creditos -> return e {modo = MenuInicial Leaderboard}
    MenuInicial Opcoes -> return e {modo = MenuInicial Creditos}
    MenuInicial Sair -> return e {modo = MenuInicial Opcoes}
    SelecionarModo -> guardarModoSelecionado e (modoJogoAnterior (modoJogoEscolhido e))
    _ -> return e
reage (EventKey (MouseButton LeftButton) Down _ posBruto) e@ImmutableTowers {modo = MenuInicial _} =
  let (mx, my) = normalizaUIPos e posBruto
   in case opcaoMenuClicada mx my of
    Just Jogar -> return (iniciarPartida e)
    Just Modos -> return e {modo = SelecionarModo}
    Just LojaMeta -> return e {modo = MostrarLojaMeta}
    Just Perfil -> return e {modo = MostrarPerfil}
    Just Leaderboard -> return e {modo = MostrarLeaderboard}
    Just Creditos -> return e {modo = TutorialFoto}
    Just Opcoes -> return e {modo = MostrarOpcoes}
    Just Sair -> exitSuccess
    Nothing -> return e
reage (EventKey (MouseButton LeftButton) Down _ posBruto) e@ImmutableTowers {modo = SelecionarModo} =
  let (mx, my) = normalizaUIPos e posBruto
   in case modoClicado mx my of
    Just modoEscolhido -> guardarModoSelecionado e modoEscolhido
    Nothing -> return e
reage (EventKey (SpecialKey KeyEnter) Down _ _) e =
  case modo e of
    MenuInicial Jogar -> return (iniciarPartida e)
    MenuInicial Modos -> return e {modo = SelecionarModo}
    MenuInicial LojaMeta -> return e {modo = MostrarLojaMeta}
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
    MostrarLojaMeta -> return e {modo = MenuInicial LojaMeta}
    SelecionarModo -> return e {modo = MenuInicial Modos}
    EditorMapa -> return e {modo = MenuInicial Modos}
    TelaVitoria -> return (reiniciarMesmoNivel e)
    TelaDerrota -> return (voltarAoMenuPosPartida e)
    _ -> return e
reage (EventKey (Char 'e') Down _ _) e =
  case modo e of
    MenuInicial _ -> return e {modo = EditorMapa}
    _ -> return e
reage (EventKey (SpecialKey KeyBackspace) Down _ _) estado = apagarCaracterPerfil True estado
reage (EventKey (SpecialKey KeyDelete) Down _ _) estado = apagarCaracterPerfil True estado
reage (EventKey (Char '\b') Down _ _) estado = apagarCaracterPerfil True estado
reage (EventKey (Char '\DEL') Down _ _) estado = apagarCaracterPerfil True estado
reage (EventKey (SpecialKey KeyBackspace) Up _ _) estado = return estado {backspacePerfilAtivo = False, backspacePerfilTimer = 0}
reage (EventKey (SpecialKey KeyDelete) Up _ _) estado = return estado {backspacePerfilAtivo = False, backspacePerfilTimer = 0}
reage (EventKey (Char c) Down _ _) estado@ImmutableTowers {modo = MostrarPerfil, perfilJogador = perfil, leaderboardLocal = leaderboard, modoJogoEscolhido = modoAtual}
  | c >= ' ' && c <= '~' && length (nomeJogador perfil) < 16 =
      let novoPerfil = perfil {nomeJogador = nomeJogador perfil ++ [c]}
       in do
            guardarMetaEstado novoPerfil leaderboard modoAtual (progressoMeta estado)
            return estado {perfilJogador = novoPerfil}
  | otherwise = return estado
reage (EventKey (SpecialKey KeyEsc) Down _ _) e =
  case modo e of
    TutorialFoto -> return e {modo = MenuInicial Creditos}
    MostrarPerfil -> return e {modo = MenuInicial Perfil}
    MostrarLeaderboard -> return e {modo = MenuInicial Leaderboard}
    MostrarOpcoes -> return e {modo = MenuInicial Opcoes}
    MostrarLojaMeta -> return e {modo = MenuInicial LojaMeta}
    SelecionarModo -> return e {modo = MenuInicial Modos}
    EditorMapa -> return e {modo = MenuInicial Modos}
    EmJogo -> return e {modo = Pausado}
    Pausado -> return e {modo = EmJogo}
    TelaVitoria -> return (voltarAoMenuPosPartida e)
    TelaDerrota -> return (voltarAoMenuPosPartida e)
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
reage (EventKey (Char 'h') Down _ _) e =
  case modo e of
    EmJogo -> return e {hudCompacto = not (hudCompacto e)}
    _ -> return e
reage (EventKey (Char 'k') Down _ _) e =
  case modo e of
    EmJogo -> return e {lojaVisivel = not (lojaVisivel e)}
    _ -> return e
reage (EventKey (Char 'a') Down _ _) e =
  case modo e of
    EmJogo -> return (toggleBotAutomatico e)
    _ -> return e
reage (EventKey (Char '1') Down _ _) e@ImmutableTowers {modo = MostrarLojaMeta, progressoMeta = meta} = do
  let (metaNovo, texto) = abrirBau BauMadeira meta
  guardarMetaEstado (perfilJogador e) (leaderboardLocal e) (modoJogoEscolhido e) metaNovo
  return $ adicionaMensagem MsgInfo texto e {progressoMeta = metaNovo}
reage (EventKey (Char '2') Down _ _) e@ImmutableTowers {modo = MostrarLojaMeta, progressoMeta = meta} = do
  let (metaNovo, texto) = abrirBau BauCristal meta
  guardarMetaEstado (perfilJogador e) (leaderboardLocal e) (modoJogoEscolhido e) metaNovo
  return $ adicionaMensagem MsgInfo texto e {progressoMeta = metaNovo}
reage (EventKey (Char '3') Down _ _) e@ImmutableTowers {modo = MostrarLojaMeta, progressoMeta = meta} = do
  let (metaNovo, texto) = abrirBau BauImperial meta
  guardarMetaEstado (perfilJogador e) (leaderboardLocal e) (modoJogoEscolhido e) metaNovo
  return $ adicionaMensagem MsgInfo texto e {progressoMeta = metaNovo}
reage (EventKey (Char 'f') Down _ _) e@ImmutableTowers {modo = MostrarLojaMeta, progressoMeta = meta} =
  case tentaFundirTempestade meta of
    Right metaNovo -> do
      guardarMetaEstado (perfilJogador e) (leaderboardLocal e) (modoJogoEscolhido e) metaNovo
      return $ adicionaMensagem MsgSucesso "Fusao concluida: Tempestade" e {progressoMeta = metaNovo}
    Left erro -> return $ adicionaMensagem MsgAviso erro e
reage (EventKey (Char 'u') Down _ _) estado@ImmutableTowers {jogo = jogoAtual, modo = EmJogo, torreFocada = foco} =
  return $ upgradeSelecionada estado jogoAtual foco
reage (EventKey (Char 's') Down _ _) estado@ImmutableTowers {jogo = jogoAtual, modo = EmJogo} = do
  guardarJogoLocal jogoAtual (registoTorres estado)
  return $ adicionaMensagem MsgSucesso "Jogo guardado" estado
reage (EventKey (Char 'l') Down _ _) estado = do
  guardado <- carregarJogoLocal
  case guardado of
    Just (jogoGuardado, registry) ->
      return
        estado
          { jogo = jogoGuardado,
            modo = EmJogo,
            torreSelecionada = Nothing,
            torreSelecionadaId = Nothing,
            torreFocada = Nothing,
            registoTorres = registry,
            efeitosUpgrade = []
          }
    Nothing -> return estado
reage (EventKey (Char 'b') Down _ _) estado@ImmutableTowers {jogo = jogoAtual, modo = EmJogo} =
  let jogoNovo = botColocaTorre jogoAtual
      registry = reconcileTowerRegistry (registoTorres estado) (torresJogo jogoNovo)
   in return $ adicionaMensagem MsgInfo "Bot sugeriu uma construcao" estado {jogo = jogoNovo, registoTorres = registry}
reage (EventKey (Char 'o') Down _ _) estado@ImmutableTowers {jogo = jogoAtual, modo = EmJogo, posicaoRato = rato} =
  return $ case rato of
    Just pos -> adicionaMensagem MsgAviso "Obstaculo colocado" estado {jogo = jogoAtual {mapaJogo = Editor.colocaObstaculo (layoutAtual estado) pos (mapaJogo jogoAtual)}}
    Nothing -> estado
reage (EventKey (MouseButton LeftButton) Down _ posBruto) estado@ImmutableTowers {jogo = jogoAtual, modo = EditorMapa} =
  let pos = normalizaUIPos estado posBruto
   in return $ case Editor.cicloCelulaMapaValidado (layoutAtual estado) pos jogoAtual of
        Right jogoNovo -> adicionaMensagem MsgSucesso "Mapa atualizado" estado {jogo = jogoNovo}
        Left erro -> adicionaMensagem MsgAviso erro estado
reage (EventKey (MouseButton RightButton) Down _ _) estado@ImmutableTowers {modo = EmJogo} =
  return estado {torreSelecionada = Nothing, torreSelecionadaId = Nothing, torreFocada = Nothing}
reage (EventKey (MouseButton LeftButton) Down _ posBruto) estado@ImmutableTowers {jogo = jogoAtual, modo = EmJogo, torreSelecionada = Nothing} =
  let pos@(mx, my) = normalizaUIPos estado posBruto
   in
  case acaoJogoClicada pos estado of
    Just novoEstado -> return novoEstado
    Nothing
      | Just novoEstado <- selecionarTorreLoja estado (mx, my) ->
          return novoEstado
      | cliqueBloqueadoPelaUI estado pos ->
          return estado {posicaoRato = Just (mx, my)}
      | otherwise ->
          case torreNoClique estado (mx, my) jogoAtual of
            Just torre -> return estado {torreFocada = Just (posicaoTorre torre), posicaoRato = Just (mx, my)}
            Nothing -> return estado {torreFocada = Nothing, posicaoRato = Just (mx, my)}
reage (EventKey (MouseButton LeftButton) Down _ posBruto) estado@ImmutableTowers {jogo = jogoAtual, modo = EmJogo, torreSelecionada = Just torre} =
  let pos@(mx, my) = normalizaUIPos estado posBruto
   in case acaoJogoClicada pos estado of
    Just novoEstado -> return novoEstado
    Nothing
      | Just novoEstado <- selecionarTorreLoja estado (mx, my) ->
          return novoEstado
      | cliqueBloqueadoPelaUI estado pos ->
          return estado {posicaoRato = Just (mx, my)}
      | otherwise ->
          return $ colocaTorreSelecionada estado jogoAtual torre (mx, my)
reage (EventKey (MouseButton LeftButton) Down _ posBruto) estado@ImmutableTowers {modo = Pausado} =
  let pos = normalizaUIPos estado posBruto
   in return $ case acaoPausaClicada pos estado of
    Just novoEstado -> novoEstado
    Nothing -> estado
reage (EventKey (MouseButton LeftButton) Down _ posBruto) estado@ImmutableTowers {modo = TelaVitoria} =
  let pos = normalizaUIPos estado posBruto
   in return $ case acaoResultadoClicada pos estado of
    Just novoEstado -> novoEstado
    Nothing -> estado
reage (EventKey (MouseButton LeftButton) Down _ posBruto) estado@ImmutableTowers {modo = TelaDerrota} =
  let pos = normalizaUIPos estado posBruto
   in return $ case acaoResultadoClicada pos estado of
    Just novoEstado -> novoEstado
    Nothing -> estado
reage (EventKey (MouseButton LeftButton) Down _ posBruto) e =
  let pos = normalizaUIPos e posBruto
   in case modo e of
        TutorialFoto -> voltaSeClicou pos submenuBackRect e
        MostrarPerfil -> voltaSeClicou pos submenuBackRect e
        MostrarLeaderboard -> voltaSeClicou pos submenuBackRect e
        MostrarOpcoes -> voltaSeClicou pos optionsBackRect e
        MostrarLojaMeta -> voltaSeClicou pos shopBackRect e
        _ -> return e
  where
    voltaSeClicou pos rect estado
      | pos `containsPoint` rect = return estado {modo = menuAnterior (modo estado)}
      | otherwise = return estado
    menuAnterior estadoAtual = case estadoAtual of
      TutorialFoto -> MenuInicial Creditos
      MostrarPerfil -> MenuInicial Perfil
      MostrarLeaderboard -> MenuInicial Leaderboard
      MostrarOpcoes -> MenuInicial Opcoes
      MostrarLojaMeta -> MenuInicial LojaMeta
      _ -> MenuInicial Jogar
reage _ estado = return estado

apagarCaracterPerfil :: Bool -> ImmutableTowers -> IO ImmutableTowers
apagarCaracterPerfil ativar estado =
  case modo estado of
    MostrarPerfil ->
      let perfil = perfilJogador estado
          nomeAtual = nomeJogador perfil
          novoPerfil = perfil {nomeJogador = maybe nomeAtual fst (unsnoc nomeAtual)}
       in do
            guardarMetaEstado novoPerfil (leaderboardLocal estado) (modoJogoEscolhido estado) (progressoMeta estado)
            return estado {perfilJogador = novoPerfil, backspacePerfilAtivo = ativar, backspacePerfilTimer = 0.22}
    _ -> return estado {backspacePerfilAtivo = False, backspacePerfilTimer = 0}

acaoJogoClicada :: (Float, Float) -> ImmutableTowers -> Maybe ImmutableTowers
acaoJogoClicada pos estado
  | pos `containsPoint` pauseRect = Just estado {modo = Pausado}
  | pos `containsPoint` speed1Rect = Just (adicionaMensagem MsgInfo "Velocidade 1x" estado {velocidadeJogo = 1})
  | pos `containsPoint` speed2Rect = Just (adicionaMensagem MsgInfo "Velocidade 2x" estado {velocidadeJogo = 2})
  | pos `containsPoint` speed4Rect = Just (adicionaMensagem MsgInfo "Velocidade 4x" estado {velocidadeJogo = 4})
  | pos `containsPoint` autoBotRect = Just (toggleBotAutomatico estado)
  | pos `containsPoint` hudToggleRect = Just estado {hudCompacto = not (hudCompacto estado)}
  | pos `containsPoint` shopToggleRect = Just estado {lojaVisivel = not (lojaVisivel estado)}
  | not (hudCompacto estado) && pos `containsPoint` specializationARect = Just (especializaSelecionada EspecializacaoA estado)
  | not (hudCompacto estado) && pos `containsPoint` specializationBRect = Just (especializaSelecionada EspecializacaoB estado)
  | not (hudCompacto estado) && pos `containsPoint` upgradeRect = Just (upgradeSelecionada estado (jogo estado) (torreFocada estado))
  | not (hudCompacto estado) && pos `containsPoint` sellRect = Just (vendeTorreSelecionada estado)
  | not (hudCompacto estado) && pos `containsPoint` cancelRect = Just estado {torreSelecionada = Nothing, torreSelecionadaId = Nothing, torreFocada = Nothing}
  | otherwise = Nothing

acaoPausaClicada :: (Float, Float) -> ImmutableTowers -> Maybe ImmutableTowers
acaoPausaClicada pos estado
  | pos `containsPoint` resumeRect = Just (adicionaMensagem MsgInfo "Continuar" estado {modo = EmJogo})
  | pos `containsPoint` restartRect = Just (adicionaMensagem MsgInfo "Partida reiniciada" (iniciarPartida estado))
  | pos `containsPoint` menuRect = Just estado {modo = MenuInicial Jogar, torreSelecionada = Nothing, torreSelecionadaId = Nothing, torreFocada = Nothing}
  | otherwise = Nothing

acaoResultadoClicada :: (Float, Float) -> ImmutableTowers -> Maybe ImmutableTowers
acaoResultadoClicada pos estado@ImmutableTowers {modo = TelaVitoria}
  | pos `containsPoint` resultMenuRect = Just (voltarAoMenuPosPartida estado)
  | pos `containsPoint` resultReplayRect = Just (reiniciarMesmoNivel estado)
  | otherwise = Nothing
acaoResultadoClicada pos estado@ImmutableTowers {modo = TelaDerrota}
  | pos `containsPoint` resultMenuRect = Just (voltarAoMenuPosPartida estado)
  | otherwise = Nothing
acaoResultadoClicada _ _ = Nothing

proximaVelocidade :: Float -> Float
proximaVelocidade velocidade
  | velocidade < 2 = 2
  | velocidade < 4 = 4
  | otherwise = 1

vendeTorreSelecionada :: ImmutableTowers -> ImmutableTowers
vendeTorreSelecionada estado =
  case torreFocada estado of
    Nothing -> adicionaMensagem MsgAviso "Seleciona uma torre primeiro" estado
    Just pos ->
      let jogoAtual = jogo estado
          (vendidas, restantes) = partition ((== pos) . posicaoTorre) (torresJogo jogoAtual)
          registry = registoTorres estado
          refund = sum [valorVendaRuntime (towerRuntimeDaTorre registry torre) torre | torre <- vendidas]
          registryAtualizado = foldr (removeTower . posicaoTorre) registry vendidas
          base = baseJogo jogoAtual
          baseAtualizada = base {creditosBase = creditosBase base + refund}
       in if null vendidas
          then adicionaMensagem MsgAviso "Nenhuma torre selecionada" estado
          else adicionaMensagem MsgSucesso ("Torre vendida +" ++ show refund) estado {jogo = jogoAtual {torresJogo = restantes, baseJogo = baseAtualizada}, registoTorres = registryAtualizado, torreFocada = Nothing}

adicionaMensagem :: TipoMensagem -> String -> ImmutableTowers -> ImmutableTowers
adicionaMensagem tipo texto estado =
  estado {mensagensUI = MensagemUI texto 2.4 tipo : take 3 (mensagensUI estado)}

guardarModoSelecionado :: ImmutableTowers -> ModoJogoEscolhido -> IO ImmutableTowers
guardarModoSelecionado e novoModo = do
  if GameFactory.modoDesbloqueado (progressoMeta e) novoModo
    then do
      guardarMetaEstado (perfilJogador e) (leaderboardLocal e) novoModo (progressoMeta e)
      return $ adicionaMensagem MsgSucesso ("Modo selecionado: " ++ nomeModoCurto novoModo) e {modoJogoEscolhido = novoModo}
    else
      return $ adicionaMensagem MsgAviso ("Modo bloqueado: nivel " ++ show (nivelMinimoModo novoModo)) e

iniciarPartida :: ImmutableTowers -> ImmutableTowers
iniciarPartida e =
  if not (GameFactory.modoDesbloqueado (progressoMeta e) (modoJogoEscolhido e))
    then adicionaMensagem MsgAviso ("Requer nivel " ++ show (nivelMinimoModo (modoJogoEscolhido e))) e
    else
      let (jogoNovo, mapaId, totalOndas, metaNovo) = prepararPartida (modoJogoEscolhido e) (progressoMeta e)
       in e
            { jogo = jogoNovo,
              modo = EmJogo,
              tempo = 0,
              torreSelecionada = Nothing,
              torreSelecionadaId = Nothing,
              torreFocada = Nothing,
              registoTorres = emptyTowerRegistry,
              progressoMeta = metaNovo,
              mapaAtual = mapaId,
              ondasSobrevividas = 0,
              totalOndasPartida = totalOndas,
              resultadoRegistado = False,
              velocidadeJogo = 1,
              mensagensUI = [],
              efeitosUpgrade = [],
              ultimoResumoPartida = Nothing,
              botAutomatico = False,
              botCooldown = 0
            }

reiniciarMesmoNivel :: ImmutableTowers -> ImmutableTowers
reiniciarMesmoNivel = iniciarPartida

voltarAoMenuPosPartida :: ImmutableTowers -> ImmutableTowers
voltarAoMenuPosPartida estado =
  estado
    { modo = MenuInicial Jogar,
      torreSelecionada = Nothing,
      torreSelecionadaId = Nothing,
      torreFocada = Nothing,
      botAutomatico = False,
      botCooldown = 0
    }

toggleBotAutomatico :: ImmutableTowers -> ImmutableTowers
toggleBotAutomatico estado =
  let ativo = not (botAutomatico estado)
      texto = if ativo then "Bot automatico ligado" else "Bot automatico desligado"
   in adicionaMensagem MsgInfo texto estado {botAutomatico = ativo, botCooldown = 0.1}

upgradeSelecionada :: ImmutableTowers -> Jogo -> Maybe Posicao -> ImmutableTowers
upgradeSelecionada estado jogoAtual foco =
  case foco >>= procurarTorreAtualizavel (torresJogo jogoAtual) of
    Nothing -> adicionaMensagem MsgAviso "Seleciona uma torre para melhorar" estado
    Just (torre, reconstruirTorres) ->
      let registry = registoTorres estado
          runtime = towerRuntimeDaTorre registry torre
          base = baseJogo jogoAtual
       in if precisaEspecializacao runtime
            then adicionaMensagem MsgAviso "Escolhe POTENCIA ou CADENCIA" estado
            else case (custoUpgradeRuntime runtime torre, upgradeTorreRuntime runtime torre) of
              (Nothing, _) -> adicionaMensagem MsgAviso "Torre no nivel maximo" estado
              (_, Nothing) -> adicionaMensagem MsgAviso "Torre no nivel maximo" estado
              (Just custo, Just torreNova)
                | creditosBase base < custo -> adicionaMensagem MsgErro "Creditos insuficientes" estado
                | otherwise ->
                    let spec = towerSpec (runtimeTowerId runtime)
                        torresAtualizadas = reconstruirTorres torreNova
                        baseAtualizada = base {creditosBase = creditosBase base - custo}
                        registryComIdentidade = insertTowerRuntime (posicaoTorre torre) runtime registry
                        registryAtualizado = upgradeTowerLevel (nivelMaximoTowerSpec spec) (posicaoTorre torre) registryComIdentidade
                        efeitoNovo = EfeitoUpgradeUI (posicaoTorre torre) 0.65
                     in adicionaMensagem MsgSucesso ("Torre melhorada para nivel " ++ show (min (nivelMaximoTowerSpec spec) (runtimeLevel runtime + 1))) estado {jogo = jogoAtual {torresJogo = torresAtualizadas, baseJogo = baseAtualizada}, registoTorres = registryAtualizado, efeitosUpgrade = efeitoNovo : take 11 (efeitosUpgrade estado)}

especializaSelecionada :: TowerSpecialization -> ImmutableTowers -> ImmutableTowers
especializaSelecionada specialization estado =
  case torreFocada estado >>= procurarTorreAtualizavel (torresJogo (jogo estado)) of
    Nothing -> adicionaMensagem MsgAviso "Seleciona uma torre para especializar" estado
    Just (torre, reconstruirTorres) ->
      let jogoAtual = jogo estado
          registry = registoTorres estado
          runtime = towerRuntimeDaTorre registry torre
          base = baseJogo jogoAtual
       in case (custoEspecializacao specialization runtime torre, upgradeComEspecializacao specialization runtime torre) of
            (Just custo, Just torreNova)
              | creditosBase base < custo -> adicionaMensagem MsgErro "Creditos insuficientes" estado
              | otherwise ->
                  let spec = towerSpec (runtimeTowerId runtime)
                      baseAtualizada = base {creditosBase = creditosBase base - custo}
                      registryComIdentidade = insertTowerRuntime (posicaoTorre torre) runtime registry
                      registryEspecializado = specializeTower specialization (posicaoTorre torre) registryComIdentidade
                      registryAtualizado = upgradeTowerLevel (nivelMaximoTowerSpec spec) (posicaoTorre torre) registryEspecializado
                      efeitoNovo = EfeitoUpgradeUI (posicaoTorre torre) 0.9
                   in adicionaMensagem MsgSucesso (nomeTorre (runtimeTowerId runtime) ++ ": " ++ nomeEspecializacao specialization) estado {jogo = jogoAtual {torresJogo = reconstruirTorres torreNova, baseJogo = baseAtualizada}, registoTorres = registryAtualizado, efeitosUpgrade = efeitoNovo : take 11 (efeitosUpgrade estado)}
            _ -> adicionaMensagem MsgAviso "Especializacao indisponivel" estado

colocaTorreSelecionada :: ImmutableTowers -> Jogo -> Torre -> (Float, Float) -> ImmutableTowers
colocaTorreSelecionada estado jogoAtual torre (mx, my) =
  let mapa = mapaJogo jogoAtual
      celula = ecraParaCelula (layoutAtual estado) mapa (mx, my)
      (posX, posY) = maybe (-1, -1) id celula
      novaPosicao = (fromIntegral posX + 0.5, fromIntegral posY + 0.5)
      terrenoValido = terrenoEmCelula mapa posX posY == Just Relva
      posicaoLivre = notElem novaPosicao (map posicaoTorre (torresJogo jogoAtual))
   in if terrenoValido && posicaoLivre
      then
        let towerId = fromMaybe (towerIdSpec (towerSpecAproximada torre)) (torreSelecionadaId estado)
            entradas = shopEntriesParaModo (modoJogoEscolhido estado) (progressoMeta estado)
            precoTorre = maybe (precoTowerSpec (towerSpec towerId)) shopPrice (find ((== towerId) . shopTowerId) entradas)
            novaBase = (baseJogo jogoAtual) {creditosBase = creditosBase (baseJogo jogoAtual) - precoTorre}
            novasTorres = torre {posicaoTorre = novaPosicao} : torresJogo jogoAtual
            registryAtualizado = registerTower towerId novaPosicao (registoTorres estado)
         in estado {jogo = jogoAtual {torresJogo = novasTorres, baseJogo = novaBase}, torreSelecionada = Nothing, torreSelecionadaId = Nothing, torreFocada = Just novaPosicao, registoTorres = registryAtualizado, posicaoRato = Just (mx, my)}
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

nomeModoCurto :: ModoJogoEscolhido -> String
nomeModoCurto modoAtual = case modoAtual of
  ModoHistoria -> "Historia"
  ModoInfinito -> "Infinito"
  ModoDesafio -> "Desafio"
  ModoBoss -> "Boss"
  ModoSandbox -> "Sandbox"

torreNoClique :: ImmutableTowers -> (Float, Float) -> Jogo -> Maybe Torre
torreNoClique estado (mx, my) jogoAtual =
  find dentro (torresJogo jogoAtual)
  where
    dentro torre =
      case (mapaParaEcra (layoutAtual estado) (mapaJogo jogoAtual) (posicaoTorre torre), tamanhoBloco (layoutAtual estado) (mapaJogo jogoAtual)) of
        (Just (sx, sy), Just bloco) -> abs (mx - sx) <= bloco * 0.45 && abs (my - sy) <= bloco * 0.45
        _ -> False

layoutAtual :: ImmutableTowers -> MapLayoutConfig
layoutAtual = layoutParaJanela . janelaAtual

normalizaUIPos :: ImmutableTowers -> (Float, Float) -> (Float, Float)
normalizaUIPos estado (mx, my) =
  let escala = min (fromIntegral (fst (janelaAtual estado)) / larguraJanela) (fromIntegral (snd (janelaAtual estado)) / alturaJanela)
   in if escala <= 0 then (mx, my) else (mx / escala, my / escala)

procurarTorreAtualizavel :: [Torre] -> Posicao -> Maybe (Torre, Torre -> [Torre])
procurarTorreAtualizavel torres alvo = go [] torres
  where
    go _ [] = Nothing
    go anteriores (torre : resto)
      | posicaoTorre torre == alvo = Just (torre, \nova -> reverse anteriores ++ nova : resto)
      | otherwise = go (torre : anteriores) resto

lojaClicada :: Int -> Float -> Float -> Int -> Bool
lojaClicada total mx my indice =
  let (posX, posY) = shopSlotCenter total indice
      largura = 82
      altura = 96
   in mx >= posX - largura / 2
        && mx <= posX + largura / 2
        && my >= posY - altura / 2
        && my <= posY + altura / 2

selecionarTorreLoja :: ImmutableTowers -> (Float, Float) -> Maybe ImmutableTowers
selecionarTorreLoja estado (mx, my)
  | not (lojaVisivel estado) = Nothing
  | otherwise =
      case find (\(i, _) -> lojaClicada (length entradas) mx my i) (zip [0 ..] entradas) of
        Just (_, entrada)
          | creditosBase (baseJogo (jogo estado)) >= shopPrice entrada ->
              let towerId = shopTowerId entrada
               in Just $ adicionaMensagem MsgInfo ("Selecionada " ++ nomeTorre towerId) estado {torreSelecionada = Just (shopTower entrada), torreSelecionadaId = Just towerId, torreFocada = Nothing, posicaoRato = Just (mx, my)}
          | otherwise ->
              Just $ adicionaMensagem MsgAviso "Creditos insuficientes" estado {posicaoRato = Just (mx, my)}
        Nothing -> Nothing
  where
    entradas = shopEntriesParaModo (modoJogoEscolhido estado) (progressoMeta estado)

cliqueBloqueadoPelaUI :: ImmutableTowers -> (Float, Float) -> Bool
cliqueBloqueadoPelaUI estado pos =
  any (containsPoint pos) hitboxes
  where
    hitboxes =
      [ UIRect 0 (alturaJanela / 2 - 46) larguraJanela 92
      , UIRect 0 (-alturaJanela / 2 + 34) larguraJanela 68
      ]
      ++ [gamePanelRect | not (hudCompacto estado)]
      ++ [shopPanelRect (length (shopEntriesParaModo (modoJogoEscolhido estado) (progressoMeta estado))) | lojaVisivel estado]

opcaoMenuClicada :: Float -> Float -> Maybe MenuInicialOpcoes
opcaoMenuClicada mx my = fmap fst $ find (containsPoint (mx, my) . snd) botoes
  where
    botoes =
      [ (Jogar, menuHeroRect),
        (Modos, menuModeRect),
        (LojaMeta, menuShopRect),
        (Perfil, menuProfileRect),
        (Leaderboard, menuLeaderboardRect),
        (Creditos, menuHelpRect),
        (Opcoes, menuOptionsRect),
        (Sair, menuExitRect)
      ]

modoClicado :: Float -> Float -> Maybe ModoJogoEscolhido
modoClicado mx my = fmap fst $ find (containsPoint (mx, my) . snd) botoes
  where
    botoes =
      [ (ModoHistoria, modeHistoriaRect),
        (ModoInfinito, modeInfinitoRect),
        (ModoDesafio, modeDesafioRect),
        (ModoBoss, modeBossRect),
        (ModoSandbox, modeSandboxRect)
      ]

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
