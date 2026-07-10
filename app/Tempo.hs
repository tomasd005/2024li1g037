module Tempo where

import BotSystem
import Data.List (sortOn)
import ImmutableTowers
import LI12425
import MapData
import MetaTypes
import ProgressionSystem
import SaveSystem
import ScoreSystem
import Tarefa3
import WaveSystem

reageTempo :: Tempo -> ImmutableTowers -> IO ImmutableTowers
reageTempo segundos estado@ImmutableTowers {modo = EmJogo} =
  let jogoAtual = jogo estado
      modoAtual = modoJogoEscolhido estado
      resultadoGuardado = resultadoRegistado estado
      velocidadeAtual = velocidadeJogo estado
      tempoAtual = tempo estado
      segundosFloat = realToFrac segundos * velocidadeAtual
      jogoAtualizado = atualizaJogo segundosFloat jogoAtual
      tempoNovo = tempoAtual + segundos
      mensagensAtualizadas = atualizaMensagens segundos (mensagensUI estado)
      efeitosAtualizados = atualizaEfeitosUpgrade segundos (efeitosUpgrade estado)
      estadoBase =
        atualizaBotAutomatico segundos $
          atualizaInputContinuo segundos estado {jogo = jogoAtualizado, tempo = tempoNovo, mensagensUI = mensagensAtualizadas, efeitosUpgrade = efeitosAtualizados}
      estadoAtualizado = autoIniciaVagaSeNecessario estadoBase
      partidaLimpa = vidaBase (baseJogo jogoAtualizado) > 0
        && null (inimigosJogo jogoAtualizado)
        && all (null . inimigosOnda) (concatMap ondasPortal (portaisJogo jogoAtualizado))
   in if partidaTerminou jogoAtualizado && not resultadoGuardado && (modoAtual /= ModoInfinito || vidaBase (baseJogo jogoAtualizado) <= 0)
      then registaResultado estadoAtualizado
      else if modoAtual == ModoInfinito && partidaLimpa
        then return (adicionaOndaInfinita estadoAtualizado)
        else return estadoAtualizado
reageTempo segundos estado =
  return $
        atualizaInputContinuo segundos $
      estado
        { tempo = tempo estado + segundos,
          mensagensUI = atualizaMensagens segundos (mensagensUI estado),
          efeitosUpgrade = atualizaEfeitosUpgrade segundos (efeitosUpgrade estado)
        }

atualizaMensagens :: Tempo -> [MensagemUI] -> [MensagemUI]
atualizaMensagens segundos =
  take 4 . filter ((> 0) . tempoMensagem) . map reduz
  where
    reduz msg = msg {tempoMensagem = tempoMensagem msg - segundos}

atualizaEfeitosUpgrade :: Tempo -> [EfeitoUpgradeUI] -> [EfeitoUpgradeUI]
atualizaEfeitosUpgrade segundos =
  filter ((> 0) . tempoEfeitoUpgrade) . map reduzir
  where
    reduzir efeito = efeito {tempoEfeitoUpgrade = tempoEfeitoUpgrade efeito - segundos}

atualizaInputContinuo :: Tempo -> ImmutableTowers -> ImmutableTowers
atualizaInputContinuo segundos estado
  | modo estado /= MostrarPerfil = estado {backspacePerfilTimer = 0}
  | not (backspacePerfilAtivo estado) = estado {backspacePerfilTimer = 0}
  | null (nomeJogador (perfilJogador estado)) = estado {backspacePerfilTimer = 0}
  | otherwise =
      let timerNovo = backspacePerfilTimer estado - segundos
       in if timerNovo > 0
            then estado {backspacePerfilTimer = timerNovo}
            else
              let perfil = perfilJogador estado
                  novoNome = if null (nomeJogador perfil) then "" else init (nomeJogador perfil)
               in estado
                    { perfilJogador = perfil {nomeJogador = novoNome},
                      backspacePerfilTimer = 0.055
                    }

atualizaBotAutomatico :: Tempo -> ImmutableTowers -> ImmutableTowers
atualizaBotAutomatico segundos estado
  | modo estado /= EmJogo = estado {botCooldown = 0}
  | not (botAutomatico estado) = estado {botCooldown = 0}
  | otherwise =
      let cooldownNovo = botCooldown estado - segundos
       in if cooldownNovo > 0
            then estado {botCooldown = cooldownNovo}
            else
              case botExecutaAcao (jogo estado) of
                Just (jogoNovo, texto) ->
                  adicionaMensagemTempo MsgInfo texto estado {jogo = jogoNovo, botCooldown = 0.85}
                Nothing ->
                  estado {botCooldown = 0.35}

autoIniciaVagaSeNecessario :: ImmutableTowers -> ImmutableTowers
autoIniciaVagaSeNecessario estado
  | modo estado /= EmJogo = estado
  | not (haVagaPendentePronta (jogo estado)) = estado
  | otherwise =
      let portaisAtualizados = map iniciaPortal (portaisJogo (jogo estado))
          jogoNovo = (jogo estado) {portaisJogo = portaisAtualizados}
       in adicionaMensagemTempo MsgInfo "Nova vaga iniciada" estado {jogo = jogoNovo}
  where
    haVagaPendentePronta jogoAtual =
      null (inimigosJogo jogoAtual)
        && any temOndaPorComecar (portaisJogo jogoAtual)
    temOndaPorComecar portal =
      case ondasPortal portal of
        [] -> False
        onda : _ -> not (null (inimigosOnda onda)) && entradaOnda onda > 0
    iniciaPortal portal =
      case ondasPortal portal of
        [] -> portal
        onda : ondas -> portal {ondasPortal = onda {entradaOnda = 0, tempoOnda = 0} : ondas}

adicionaMensagemTempo :: TipoMensagem -> String -> ImmutableTowers -> ImmutableTowers
adicionaMensagemTempo tipo texto estado =
  estado {mensagensUI = MensagemUI texto 2.4 tipo : take 3 (mensagensUI estado)}

partidaTerminou :: Jogo -> Bool
partidaTerminou jogoAtual =
  vidaBase (baseJogo jogoAtual) <= 0
    || (null (inimigosJogo jogoAtual) && all (null . inimigosOnda) (concatMap ondasPortal (portaisJogo jogoAtual)))

adicionaOndaInfinita :: ImmutableTowers -> ImmutableTowers
adicionaOndaInfinita estado =
  let proxima = ondasSobrevividas estado + 1
      jogoAtual = jogo estado
      novaOnda = criaOnda (proxima + 1) (5 + proxima) 2
      portaisAtualizados = case portaisJogo jogoAtual of
        [] -> [(portalPorMapa (mapaAtual estado)) {ondasPortal = [novaOnda]}]
        (portal:resto) -> portal {ondasPortal = [novaOnda]} : resto
   in estado {jogo = jogoAtual {portaisJogo = portaisAtualizados}, ondasSobrevividas = proxima, totalOndasPartida = proxima + 1}

registaResultado :: ImmutableTowers -> IO ImmutableTowers
registaResultado estado = do
  let ganhou = vidaBase (baseJogo (jogo estado)) > 0
      score = pontuacaoAtual estado
      perfil = perfilJogador estado
      metaAtual = progressoMeta estado
      perfilAtualizado = perfil
        { jogosJogador = jogosJogador perfil + 1,
          vitoriasJogador = vitoriasJogador perfil + if ganhou then 1 else 0,
          derrotasJogador = derrotasJogador perfil + if ganhou then 0 else 1,
          melhorPontuacaoJogador = max (melhorPontuacaoJogador perfil) score
        }
      metaComRecompensa
        | not ganhou = metaAtual
        | modoJogoEscolhido estado == ModoHistoria =
            let baseMeta = avancaHistoria metaAtual
             in baseMeta {gemasJogador = gemasJogador baseMeta + recompensaVitoriaModo (modoJogoEscolhido estado) metaAtual}
        | otherwise =
            metaAtual
              { gemasJogador = gemasJogador metaAtual + recompensaVitoriaModo (modoJogoEscolhido estado) metaAtual,
                nivelJogadorMeta = max (nivelJogadorMeta metaAtual) (1 + (estagiosConcluidos metaAtual + vitoriasJogador perfilAtualizado) `div` 2)
              }
      resumo =
        ResumoPartida
          { resultadoPartidaResumo = if ganhou then PartidaVitoria else PartidaDerrota,
            tempoPartidaResumo = tempo estado,
            pontuacaoPartidaResumo = score,
            ondasPartidaResumo = max (ondasSobrevividas estado) (totalOndasPartida estado),
            creditosPartidaResumo = creditosBase (baseJogo (jogo estado)),
            torresPartidaResumo = length (torresJogo (jogo estado)),
            mapaPartidaResumo = mapaAtual estado,
            modoPartidaResumo = modoJogoEscolhido estado
          }
      entrada = Pontuacao (nomeJogador perfil) (modoJogoEscolhido estado) score (max (ondasSobrevividas estado) (totalOndasPartida estado))
      leaderboardAtualizada = take 10 $ sortOn (negate . valorPontuacao) (entrada : leaderboardLocal estado)
  guardarMetaEstado perfilAtualizado leaderboardAtualizada (modoJogoEscolhido estado) metaComRecompensa
  return
    estado
      { perfilJogador = perfilAtualizado,
        leaderboardLocal = leaderboardAtualizada,
        progressoMeta = metaComRecompensa,
        resultadoRegistado = True,
        ultimoResumoPartida = Just resumo,
        modo = if ganhou then TelaVitoria else TelaDerrota,
        botAutomatico = False,
        botCooldown = 0
      }
