module Main where

import Desenhar
import Eventos
import Graphics.Gloss.Interface.IO.Game
import ImmutableTowers
import Tempo

-- | A função @main@ é o ponto de entrada do programa.
main :: IO ()
main = do
  putStrLn "==================================="
  putStrLn "  IMMUTABLE TOWERS - Tower Defense"
  putStrLn "==================================="
  putStrLn ""
  putStrLn "Controles:"
  putStrLn "  - Use SETAS ou CLIQUE para navegar no menu"
  putStrLn "  - ENTER ou CLIQUE para confirmar"
  putStrLn "  - P para pausar/retomar"
  putStrLn "  - ESC para sair"
  putStrLn "  - Botao DIREITO para cancelar selecao de torre"
  putStrLn ""
  putStrLn "Como jogar:"
  putStrLn "  1. Clique nas torres na parte inferior para comprar"
  putStrLn "  2. Clique em terreno de RELVA (verde) para colocar"
  putStrLn "  3. Defenda a base dos inimigos!"
  putStrLn ""
  putStrLn "Iniciando jogo..."
  putStrLn "==================================="
  
  imgs <- carregarImagens
  playIO janela corFundo frameRate imgs desenha reage reageTempo

-- | A constante @janela@ define o tipo de exibição da janela do jogo.
janela :: Display
janela = InWindow "Immutable Towers" (round larguraJanela, round alturaJanela) (100, 50)

-- | A constante @corFundo@ define a cor de fundo da janela do jogo.
corFundo :: Color
corFundo = black

-- | A constante @frameRate@ define a taxa de quadros por segundo para o jogo.
frameRate :: Int
frameRate = 60