module SaveSystem
  ( carregarMetaEstado,
    guardarMetaEstado,
    guardarJogoLocal,
    carregarJogoLocal,
  )
where

import Control.Exception (IOException, try)
import ImmutableTowers
import LI12425
import MetaTypes
import Text.Read (readMaybe)

caminhoMetaEstado :: FilePath
caminhoMetaEstado = "immutable-towers-meta.txt"

caminhoSaveJogo :: FilePath
caminhoSaveJogo = "immutable-towers-save.txt"

carregarMetaEstado :: IO (PerfilJogador, [Pontuacao], ModoJogoEscolhido, MetaProgress)
carregarMetaEstado = do
  resultado <- try (readFile caminhoMetaEstado) :: IO (Either IOException String)
  case resultado of
    Right conteudo ->
      case (readMaybe conteudo :: Maybe (PerfilJogador, [Pontuacao], ModoJogoEscolhido, MetaProgress)) of
        Just meta -> return meta
        Nothing ->
          case (readMaybe conteudo :: Maybe (PerfilJogador, [Pontuacao], ModoJogoEscolhido)) of
            Just (perfil, leaderboard, modoAtual) -> return (perfil, leaderboard, modoAtual, progressoInicial)
            Nothing -> return metaInicial
    Left _ -> return metaInicial
  where
    metaInicial = (perfilInicial, [], ModoHistoria, progressoInicial)

guardarMetaEstado :: PerfilJogador -> [Pontuacao] -> ModoJogoEscolhido -> MetaProgress -> IO ()
guardarMetaEstado perfil leaderboard modoAtual meta =
  writeFile caminhoMetaEstado (show (perfil, leaderboard, modoAtual, meta))

guardarJogoLocal :: Jogo -> IO ()
guardarJogoLocal jogoAtual = writeFile caminhoSaveJogo (show jogoAtual)

carregarJogoLocal :: IO (Maybe Jogo)
carregarJogoLocal = do
  resultado <- try (readFile caminhoSaveJogo) :: IO (Either IOException String)
  case resultado of
    Right conteudo -> return (readMaybe conteudo)
    Left _ -> return Nothing
