module ImmutableTowers where

import Graphics.Gloss
import LI12425

-- | A estrutura @ImmutableTowers@ representa o estado completo do jogo.
--
-- Inclui o estado atual do jogo (estrutura do tipo 'Jogo'), imagens carregadas, modo de jogo atual (menu, em jogo, etc), o tempo desde o início do jogo, e a torre atualmente selecionada (se existir).
type Imagens = [(Imagem, Maybe Picture)]

data ImmutableTowers = ImmutableTowers
  { jogo :: Jogo,
    imagens :: Imagens,
    modo :: Modo,
    tempo :: Float,
    torreSelecionada :: Maybe Torre,
    posicaoRato :: Maybe (Float, Float)
  }

-- | O tipo @Imagem@ representa os identificadores únicos das imagens utilizadas no jogo.
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

-- | Tipo @MenuInicialOpcoes@ representa as opções disponíveis no menu inicial.
data MenuInicialOpcoes = Jogar | Creditos | Sair deriving (Show, Eq)

-- | O tipo @Modo@ representa os diferentes estados/modos em que o jogo pode estar.
data Modo
  = MenuInicial MenuInicialOpcoes
  | EmJogo
  | TutorialFoto
  | MostrarCreditos
  | Pausado
  deriving (Show, Eq)

-- | A função @getImagem@ procura uma imagem no dicionário de imagens carregadas.
--
-- Se a imagem não for encontrada ou estiver em 'Nothing', retorna uma imagem em branco ('Blank').
--
-- == Exemplos:
--
-- >>> getImagem Fundo imagens
-- <Picture correspondente ao fundo, se existir>
getImagem :: Imagem -> Imagens -> Picture
getImagem k d = case lookup k d of
  Just (Just img) -> img
  _ -> Blank

-- | Igual a 'getImagem', mas desenha um marcador visível quando um asset
-- essencial não foi carregado. Útil para descobrir imagens em falta sem deixar
-- elementos importantes invisíveis durante o desenvolvimento.
getImagemOuFallback :: Imagem -> Imagens -> Picture
getImagemOuFallback k d = case lookup k d of
  Just (Just img) -> img
  _ -> Pictures
        [ Color (withAlpha 0.35 red) $ rectangleSolid 32 32
        , Color red $ rectangleWire 32 32
        ]

-- | O mapa principal do jogo, definido como uma matriz de terrenos.
--
-- Cada célula pode ser 'Relva', 'Terra' ou 'Agua'. Este mapa define o cenário e caminhos disponíveis para inimigos e posicionamento de torres.
mapa01 :: [[Terreno]]
mapa01 =
  [ [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, a, a, a, a, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, a, a, a, a, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, a, a, a, a, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, a, a, a, a, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, a, a, a, a, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r]
  ]
  where
    t = Terra
    r = Relva
    a = Agua

onda02, onda03 :: Onda
onda02 = onda01 -- por agora, clones da onda01
onda03 = onda01

-- | Base inicial do jogador. Define a posição, os créditos iniciais e a vida.
base01 :: Base
base01 = Base {posicaoBase = (35.93347, 22.000067), creditosBase = 150, vidaBase = 80}

-- | Portal de onde os inimigos emergem, contendo três ondas de ataque.
portal01 :: Portal
portal01 = Portal {ondasPortal = [onda01, onda02, onda03], posicaoPortal = (0, 2)}

-- | Primeira onda de inimigos. Aparece ao fim de 10 ciclos, com um intervalo de 10 segundos.
onda01 :: Onda
onda01 = Onda {inimigosOnda = [inimigo02_onda, inimigo01_onda], cicloOnda = 10, tempoOnda = 10, entradaOnda = 0}

inimigo01_onda = Inimigo {posicaoInimigo = (0, 2), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 1, ataqueInimigo = 10, butimInimigo = 20, projeteisInimigo = []}

-- | Inimigo padrão, com velocidade 4 e dano 10.
inimigo02_onda :: Inimigo
inimigo02_onda = Inimigo {posicaoInimigo = (0.5, 2), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 4, ataqueInimigo = 10, butimInimigo = 20, projeteisInimigo = []}

-- | Tamanho de cada célula em pixeis (32x32).
pixeis :: Int
pixeis = 32

-- | Largura da janela do jogo (em pixeis).
larguraJanela :: Float
larguraJanela = 1152

-- | Altura da janela do jogo (em pixeis).
alturaJanela :: Float
alturaJanela = 1080
