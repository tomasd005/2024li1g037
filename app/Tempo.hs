module Tempo where

import Data.List (sortOn)
import ImmutableTowers
import LI12425
import MapData
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
      estadoAtualizado = estado {jogo = jogoAtualizado, tempo = tempoNovo, mensagensUI = mensagensAtualizadas}
      partidaLimpa = vidaBase (baseJogo jogoAtualizado) > 0
        && null (inimigosJogo jogoAtualizado)
        && all (null . inimigosOnda) (concatMap ondasPortal (portaisJogo jogoAtualizado))
   in if partidaTerminou jogoAtualizado && not resultadoGuardado && (modoAtual /= ModoInfinito || vidaBase (baseJogo jogoAtualizado) <= 0)
      then registaResultado estadoAtualizado
      else if modoAtual == ModoInfinito && partidaLimpa
        then return (adicionaOndaInfinita estadoAtualizado)
        else return estadoAtualizado
reageTempo _ estado = return estado

atualizaMensagens :: Tempo -> [MensagemUI] -> [MensagemUI]
atualizaMensagens segundos =
  take 4 . filter ((> 0) . tempoMensagem) . map reduz
  where
    reduz msg = msg {tempoMensagem = tempoMensagem msg - segundos}

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
        [] -> [portalBase {ondasPortal = [novaOnda]}]
        (portal:resto) -> portal {ondasPortal = [novaOnda]} : resto
   in estado {jogo = jogoAtual {portaisJogo = portaisAtualizados}, ondasSobrevividas = proxima}

registaResultado :: ImmutableTowers -> IO ImmutableTowers
registaResultado estado = do
  let ganhou = vidaBase (baseJogo (jogo estado)) > 0
      score = pontuacaoAtual estado
      perfil = perfilJogador estado
      perfilAtualizado = perfil
        { jogosJogador = jogosJogador perfil + 1,
          vitoriasJogador = vitoriasJogador perfil + if ganhou then 1 else 0,
          derrotasJogador = derrotasJogador perfil + if ganhou then 0 else 1,
          melhorPontuacaoJogador = max (melhorPontuacaoJogador perfil) score
        }
      entrada = Pontuacao (nomeJogador perfil) (modoJogoEscolhido estado) score (ondasSobrevividas estado)
      leaderboardAtualizada = take 10 $ sortOn (negate . valorPontuacao) (entrada : leaderboardLocal estado)
  guardarMetaEstado perfilAtualizado leaderboardAtualizada (modoJogoEscolhido estado)
  return estado {perfilJogador = perfilAtualizado, leaderboardLocal = leaderboardAtualizada, resultadoRegistado = True}
