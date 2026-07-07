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
import Text.Read (readMaybe)

caminhoMetaEstado :: FilePath
caminhoMetaEstado = "immutable-towers-meta.txt"

caminhoSaveJogo :: FilePath
caminhoSaveJogo = "immutable-towers-save.txt"

carregarMetaEstado :: IO (PerfilJogador, [Pontuacao], ModoJogoEscolhido)
carregarMetaEstado = do
  resultado <- try (readFile caminhoMetaEstado) :: IO (Either IOException String)
  case resultado of
    Right conteudo -> case readMaybe conteudo of
      Just meta -> return meta
      Nothing -> return metaInicial
    Left _ -> return metaInicial
  where
    metaInicial = (perfilInicial, [], ModoHistoria)

guardarMetaEstado :: PerfilJogador -> [Pontuacao] -> ModoJogoEscolhido -> IO ()
guardarMetaEstado perfil leaderboard modoAtual =
  writeFile caminhoMetaEstado (show (perfil, leaderboard, modoAtual))

guardarJogoLocal :: Jogo -> IO ()
guardarJogoLocal jogoAtual = writeFile caminhoSaveJogo (show jogoAtual)

carregarJogoLocal :: IO (Maybe Jogo)
carregarJogoLocal = do
  resultado <- try (readFile caminhoSaveJogo) :: IO (Either IOException String)
  case resultado of
    Right conteudo -> return (readMaybe conteudo)
    Left _ -> return Nothing
