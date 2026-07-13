module SaveSystem
  ( carregarMetaEstado,
    guardarMetaEstado,
    guardarJogoLocal,
    carregarJogoLocal,
    encodeGameSave,
    decodeGameSave,
  )
where

import Control.Exception (IOException, try)
import ImmutableTowers
import LI12425
import MetaTypes
import Text.Read (readMaybe)
import TowerRuntime
import TowerSystem (registryFromLegacy)

data GameSave
  = GameSaveV2 Jogo TowerRegistry
  deriving (Show, Read)

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

guardarJogoLocal :: Jogo -> TowerRegistry -> IO ()
guardarJogoLocal jogoAtual registry =
  writeFile caminhoSaveJogo (encodeGameSave jogoAtual registry)

encodeGameSave :: Jogo -> TowerRegistry -> String
encodeGameSave jogoAtual registry = show (GameSaveV2 jogoAtual registry)

decodeGameSave :: String -> Maybe (Jogo, TowerRegistry)
decodeGameSave conteudo =
  case readMaybe conteudo :: Maybe GameSave of
    Just (GameSaveV2 jogoAtual registry) -> Just (jogoAtual, registry)
    Nothing -> do
      jogoLegado <- readMaybe conteudo :: Maybe Jogo
      return (jogoLegado, registryFromLegacy (torresJogo jogoLegado))

carregarJogoLocal :: IO (Maybe (Jogo, TowerRegistry))
carregarJogoLocal = do
  resultado <- try (readFile caminhoSaveJogo) :: IO (Either IOException String)
  return $ case resultado of
    Right conteudo -> decodeGameSave conteudo
    Left _ -> Nothing
