-- |
-- Module      : Tarefa2
-- Description : Auxiliares do Jogo
-- Copyright   : Tomás Branco Dias <a107323@alunos.uminho.pt>
--               Ines Braga da Silva <a112819@alunos.uminho.pt>
--
-- Módulo para a realização da Tarefa 2 de LI1 em 2024/25. Este módulo implementa funções auxiliares que são usadas para o desenvolvimento da mecânica de jogo.
module Tarefa2 where

import Data.List (find)
import LI12425

projetil1 :: Projetil
projetil1 = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}

projetil2 :: Projetil
projetil2 = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}

projetil3 :: Projetil
projetil3 = Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}

torre2 :: Torre
torre2 = Torre {posicaoTorre = (0, 1), alcanceTorre = 2.0, rajadaTorre = 3, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 2.0, projetilTorre = projetil1}

inimigo2 :: Inimigo
inimigo2 =
  Inimigo
    { posicaoInimigo = (1.0, 0.0),
      direcaoInimigo = Oeste,
      vidaInimigo = 80.0,
      velocidadeInimigo = 0.5,
      ataqueInimigo = 15.0,
      butimInimigo = 70,
      projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}, Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]
    }

-- | A função inimigosNoAlcance calcula os inimigos ao alcance de uma dada torre.
--
-- == Exemplo:
-- >>> inimigosNoAlcance torre2 []
-- []
-- >>> inimigosNoAlcance torre2 [inimigo1]
-- [Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = []}]
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance Torre {posicaoTorre = (x, y), alcanceTorre = alcance} inimigos =
  filter dentroDoAlcance inimigos
  where
    alcanceQuadrado = alcance * alcance
    dentroDoAlcance Inimigo {posicaoInimigo = (a, b)} =
      let dx = x - a
          dy = y - b
       in dx * dx + dy * dy <= alcanceQuadrado

-- | A função atingeInimigo atualiza o estado de um inimigo assumindo que este acaba de ser atingido por um projetil de uma torre.
-- A vida do inimigo diminui tanto quanto o dano que o projetil da torre causa, e as sinergias entre projéteis são aplicadas.
--
-- == Exemplo:
-- >>> atingeInimigo torre2 inimigo1
-- Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Este, vidaInimigo = 99.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}]}
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo Torre {danoTorre = dano, projetilTorre = projetilNovo} inimigo@Inimigo {vidaInimigo = vida, projeteisInimigo = projeteis} =
  let projeteisAtualizados = aplicaSinergias (atualizarProjetis projetilNovo projeteis)
      danoExtra = danoSinergia projeteisAtualizados
   in inimigo
        { vidaInimigo = max 0 (vida - dano - danoExtra),
          projeteisInimigo = projeteisAtualizados
        }

-- | A função fogoEGelo remove projeteis de Fogo e Gelo ativos simultaneamente.
--
-- == Exemplo:
-- >>> fogoEGelo [projetil1, projetil2, projetil3]
-- [Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}]
-- >>> fogoEGelo []
-- []
fogoEGelo :: [Projetil] -> [Projetil]
fogoEGelo projeteis =
  if not (null (filter (\p -> tipoProjetil p == Fogo) projeteis)) && not (null (filter (\p -> tipoProjetil p == Gelo) projeteis))
    then filter (\p -> tipoProjetil p /= Fogo && tipoProjetil p /= Gelo) projeteis
    else projeteis

-- | Devolve uma lista com apenas projeteis do tipo Gelo presentes na lista.
--
-- == Exemplo:
-- >>> encontraGelo [projetil2, projetil3, projetil2]
-- [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]
-- >>> encontraGelo []
-- []
encontraGelo :: [Projetil] -> [Projetil]
encontraGelo = filter (\p -> tipoProjetil p == Gelo)

-- | A função atingeFogoEResina remove projeteis de Resina e dobra a duração de projeteis de Fogo quando ambos estão ativos.
--
-- == Exemplo:
-- >>> atingeFogoEResina [projetil1, projetil3, projetil2]
-- [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 10.0}]
-- >>> atingeFogoEResina [projetil2]
-- [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]
atingeFogoEResina :: [Projetil] -> [Projetil]
atingeFogoEResina projeteis =
  case (find (\p -> tipoProjetil p == Fogo) projeteis, find (\p -> tipoProjetil p == Resina) projeteis) of
    (Just fogo, Just _) -> [Projetil Fogo (duplicaDuracao (duracaoProjetil fogo))]
    _ -> projeteis

-- | Devolve uma lista com apenas projeteis do tipo Fogo presentes na lista.
--
-- == Exemplo:
-- >>> encontraFogo [projetil1, projetil2, projetil1]
-- [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}]
-- >>> encontraFogo []
-- []
encontraFogo :: [Projetil] -> [Projetil]
encontraFogo = filter (\p -> tipoProjetil p == Fogo)

-- | Devolve uma lista com apenas projeteis do tipo Resina presentes na lista.
--
-- == Exemplo:
-- >>> encontraResina [projetil3, projetil2, projetil3]
-- [Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0},Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}]
-- >>> encontraResina []
-- []
encontraResina :: [Projetil] -> [Projetil]
encontraResina = filter (\p -> tipoProjetil p == Resina)

-- | Dobra a duração de um projetil.
--
-- == Exemplo:
-- >>> duplicaDuracao (Finita 5.0)
-- Finita 10.0
-- >>> duplicaDuracao Infinita
-- Infinita
duplicaDuracao :: Duracao -> Duracao
duplicaDuracao (Finita t) = Finita (2 * t)
duplicaDuracao Infinita = Infinita

-- | Divide uma lista de projeteis em três listas diferentes de acordo com o tipo de projetil.
--
-- == Exemplo:
-- >>> dividePorTipoProjetil [projetil1, projetil2, projetil3]
-- ([Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}],[Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}],[Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}])
-- >>> dividePorTipoProjetil []
-- ([],[],[])
dividePorTipoProjetil :: [Projetil] -> ([Projetil], [Projetil], [Projetil])
dividePorTipoProjetil projeteis = (fogos, gelos, resinas)
  where
    fogos = filter (\p -> tipoProjetil p == Fogo) projeteis
    gelos = filter (\p -> tipoProjetil p == Gelo) projeteis
    resinas = filter (\p -> tipoProjetil p == Resina) projeteis

aplicaSinergias :: [Projetil] -> [Projetil]
aplicaSinergias projeteis =
  let tipos = map tipoProjetil projeteis
      base = if Fogo `elem` tipos && Resina `elem` tipos
             then atingeFogoEResina projeteis
             else fogoEGelo projeteis
      extra =
        [Projetil Eletrico (Finita 1.2) | Gelo `elem` tipos && Eletrico `elem` tipos]
          ++ [Projetil Medo (Finita 2.5) | Medo `elem` tipos && Veneno `elem` tipos]
   in somaProjetil (base ++ extra)

danoSinergia :: [Projetil] -> Float
danoSinergia projeteis =
  let tipos = map tipoProjetil projeteis
   in sum
        [8 | Fogo `elem` tipos && Eletrico `elem` tipos]
          + sum [5 | Veneno `elem` tipos && Resina `elem` tipos]

-- | Soma as durações de uma lista de projeteis.
--
-- == Exemplo:
-- >>> somaDuracoes [Finita 5.0, Finita 3.0, Finita 4.0]
-- Finita 12.0
-- >>> somaDuracoes [Finita 5.0, Infinita]
-- Infinita
somaDuracoes :: [Duracao] -> Duracao
somaDuracoes [] = Finita 0
somaDuracoes (Infinita : _) = Infinita
somaDuracoes (Finita t : ds) = case somaDuracoes ds of
  Finita restante -> Finita (t + restante)
  Infinita -> Infinita

-- | Verifica se existem projeteis repetidos de tipo igual.
--
-- == Exemplo:
-- >>> verificaIguais [projetil1, projetil2, projetil1]
-- True
-- >>> verificaIguais [projetil2, projetil3]
-- False
verificaIguais :: [Projetil] -> Bool
verificaIguais projeteis =
  any (\l -> length l > 1) [fogos, gelos, resinas]
  where
    (fogos, gelos, resinas) = dividePorTipoProjetil projeteis

-- | Agrupa projéteis do mesmo tipo e soma as suas durações.
--
-- == Exemplo:
-- >>> somaProjetil [Projetil Fogo (Finita 5.0), Projetil Fogo (Finita 5.0), Projetil Gelo (Finita 3.0)]
-- [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 10.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]
-- >>> somaProjetil []
-- []
somaProjetil :: [Projetil] -> [Projetil]
somaProjetil [] = []
somaProjetil (p : ps) =
  let mesmosTipo = filter (\x -> tipoProjetil x == tipoProjetil p) ps
      duracaoTotal = somaDuracoes (duracaoProjetil p : map duracaoProjetil mesmosTipo)
      restantes = filter (\x -> tipoProjetil x /= tipoProjetil p) ps
   in Projetil (tipoProjetil p) duracaoTotal : somaProjetil restantes

-- | A função ativaInimigo move o próximo inimigo a ser lançado por um portal para a lista de inimigos ativos.
--
-- == Exemplo:
-- >>> ativaInimigo portal1 []
-- (Portal {posicaoPortal = (0.0,0.0), ondasPortal = []},[])
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal@Portal {ondasPortal = []} inimigos = (portal, inimigos)
ativaInimigo portal@Portal {ondasPortal = (Onda {inimigosOnda = []} : _)} inimigos =
  (portal, inimigos)
ativaInimigo portal@Portal {ondasPortal = (Onda {inimigosOnda = (i : is), cicloOnda = ciclo, tempoOnda = tempo, entradaOnda = entrada} : outrasOndas)} inimigos =
  let novaOnda = Onda {inimigosOnda = is, cicloOnda = ciclo, tempoOnda = tempo, entradaOnda = entrada}
      novasOndas = if null is then outrasOndas else novaOnda : outrasOndas
   in (portal {ondasPortal = novasOndas}, inimigos ++ [i])

-- |
-- A função 'removeEfeito' remove um efeito específico aplicado a um inimigo, caso ele esteja presente na sua lista de projéteis.
--
-- Ela verifica se há algum projétil na lista do inimigo correspondente ao tipo especificado e o remove.
-- Caso o tipo do projétil não esteja presente na lista, a função retorna o inimigo sem alterações.
--
-- == Exemplo:
--
-- >>> removeEfeito Fogo inimigo1
-- Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = []}
--
-- >>> removeEfeito Gelo inimigo2
-- Inimigo {posicaoInimigo = (1.0,0.0), direcaoInimigo = Oeste, vidaInimigo = 80.0, velocidadeInimigo = 0.5, ataqueInimigo = 15.0, butimInimigo = 70, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}]}
removeEfeito :: TipoProjetil -> Inimigo -> Inimigo
removeEfeito tipo inimigo@Inimigo {projeteisInimigo = projeteis} =
  let projeteisAtualizados = filter (\p -> tipoProjetil p /= tipo) projeteis
   in inimigo {projeteisInimigo = projeteisAtualizados}

-- | A função 'atualizarProjetis' adiciona um novo projétil ao fim da lista de
-- projéteis ativos do inimigo, preservando os efeitos já existentes.
--
-- == Exemplo:
--
-- >>> atualizarProjetis (Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}) [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}]
-- [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0},Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}]
--
-- >>> atualizarProjetis (Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}) [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}]
-- [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0},Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}]
--
-- >>> atualizarProjetis (Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}) [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}]
-- [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0},Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3.0}]
-- -- Não há interações especiais entre Fogo e Gelo, então ambos são mantidos na lista sem modificações.
--
-- >>> atualizarProjetis (Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}) [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}]
-- [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4.0}]
--
-- >>> atualizarProjetis (Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}) [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}]
-- [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0},Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3.0}]
atualizarProjetis :: Projetil -> [Projetil] -> [Projetil]
atualizarProjetis projetilNovo projeteis =
  let projeteisCompatíveis = case tipoProjetil projetilNovo of
        Fogo -> filter (\p -> tipoProjetil p /= Resina) projeteis
        Eletrico -> filter (\p -> tipoProjetil p /= Gelo) projeteis
        _ -> projeteis
   in projeteisCompatíveis ++ [projetilNovo]

-- | A função terminouJogo decide se o jogo terminou, ou seja, se o jogador ganhou ou perdeu o jogo.
--
-- == Exemplo:
-- >>> terminouJogo jogo1
-- False
-- >>> terminouJogo jogo1 {baseJogo = base1 {vidaBase = 0}}
-- True
--
-- Nota: No exemplo 1 o jogo continua, no exemplo 2 foi uma derrota.
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo

ganhouJogo :: Jogo -> Bool
ganhouJogo Jogo {baseJogo = Base {vidaBase = x}, inimigosJogo = inimigos} = x > 0 && null inimigos

perdeuJogo :: Jogo -> Bool
perdeuJogo Jogo {baseJogo = Base {vidaBase = x}} = x <= 0
