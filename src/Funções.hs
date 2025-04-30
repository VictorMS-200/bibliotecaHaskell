module Funções where

import Tipos

adicionarLivro :: Livro -> [Livro] -> [Livro]
adicionarLivro novoLivro listaDeLivro = novoLivro : listaDeLivro

verificaLivro :: [Livro] -> Int -> Maybe ()
verificaLivro listaDeLivro idProcurado
    | null listaDeLivro = Just ()
    | idLivro (head listaDeLivro) == idProcurado = Nothing
    | otherwise = verificaLivro (tail listaDeLivro) idProcurado