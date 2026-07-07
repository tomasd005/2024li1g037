module ImmutableTowers where

import Graphics.Gloss
import LI12425

type Imagens = [(Imagem, Maybe Picture)]

data ImmutableTowers = ImmutableTowers
  { jogo :: Jogo,
    imagens :: Imagens,
    modo :: Modo,
    tempo :: Float,
    torreSelecionada :: Maybe Torre,
    torreFocada :: Maybe Posicao,
    posicaoRato :: Maybe (Float, Float),
    perfilJogador :: PerfilJogador,
    leaderboardLocal :: [Pontuacao],
    modoJogoEscolhido :: ModoJogoEscolhido,
    ondasSobrevividas :: Int,
    resultadoRegistado :: Bool,
    velocidadeJogo :: Float,
    mensagensUI :: [MensagemUI]
  }

data TipoMensagem = MsgInfo | MsgSucesso | MsgAviso | MsgErro
  deriving (Show, Read, Eq)

data MensagemUI = MensagemUI
  { textoMensagem :: String,
    tempoMensagem :: Float,
    tipoMensagem :: TipoMensagem
  }
  deriving (Show, Read, Eq)

data PerfilJogador = PerfilJogador
  { nomeJogador :: String,
    jogosJogador :: Int,
    vitoriasJogador :: Int,
    derrotasJogador :: Int,
    melhorPontuacaoJogador :: Int
  }
  deriving (Show, Read, Eq)

data Pontuacao = Pontuacao
  { nomePontuacao :: String,
    modoPontuacao :: ModoJogoEscolhido,
    valorPontuacao :: Int,
    ondasPontuacao :: Int
  }
  deriving (Show, Read, Eq)

data ModoJogoEscolhido
  = ModoHistoria
  | ModoInfinito
  | ModoDesafio
  | ModoBoss
  | ModoSandbox
  deriving (Show, Read, Eq)

perfilInicial :: PerfilJogador
perfilInicial =
  PerfilJogador
    { nomeJogador = "Jogador",
      jogosJogador = 0,
      vitoriasJogador = 0,
      derrotasJogador = 0,
      melhorPontuacaoJogador = 0
    }

data Imagem
  = Fundo
  | Play
  | Exit
  | Grass
  | Water
  | Land
  | ButaoCreditos
  | ImagemTutorial
  | Inimigocima
  | BaseFoto
  | TorreFoto
  | TorreGelo
  | TorreFogo
  | TorreResina
  | PortalFoto
  | Vitoria
  | Derrota
  | ImagemCreditos
  deriving (Show, Eq)

data MenuInicialOpcoes
  = Jogar
  | Modos
  | Perfil
  | Leaderboard
  | Creditos
  | Opcoes
  | Sair
  deriving (Show, Eq)

data Modo
  = MenuInicial MenuInicialOpcoes
  | EmJogo
  | TutorialFoto
  | MostrarCreditos
  | MostrarPerfil
  | MostrarLeaderboard
  | MostrarOpcoes
  | SelecionarModo
  | EditorMapa
  | Pausado
  deriving (Show, Eq)

getImagem :: Imagem -> Imagens -> Picture
getImagem k d = case lookup k d of
  Just (Just img) -> img
  _ -> Blank

getImagemOuFallback :: Imagem -> Imagens -> Picture
getImagemOuFallback k d = case lookup k d of
  Just (Just img) -> img
  _ ->
    Pictures
      [ Color (withAlpha 0.35 red) $ rectangleSolid 32 32,
        Color red $ rectangleWire 32 32
      ]

pixeis :: Int
pixeis = 32

larguraJanela :: Float
larguraJanela = 1152

alturaJanela :: Float
alturaJanela = 1080
