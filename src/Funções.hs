module Funções where

import Tipos

adicionarLivro :: Livro -> [Livro] -> [Livro]
adicionarLivro novoLivro listaDeLivro = novoLivro : listaDeLivro

removerLivro :: [Livro] -> Int -> [Livro]
removerLivro listaDeLivro idProcurado
    | null listaDeLivro = []
    | idLivro (head listaDeLivro) == idProcurado = tail listaDeLivro
    | otherwise = head listaDeLivro : removerLivro (tail listaDeLivro) idProcurado

verificaLivro :: [Livro] -> Int -> Maybe ()
verificaLivro listaDeLivro idProcurado
    | null listaDeLivro = Just ()
    | idLivro (head listaDeLivro) == idProcurado = Nothing
    | otherwise = verificaLivro (tail listaDeLivro) idProcurado

adicionarUsuario :: Usuario -> [Usuario] -> [Usuario]
adicionarUsuario novoUsuario listaDeUsuario = novoUsuario : listaDeUsuario

verificaUsuario :: [Usuario] -> Int -> Maybe ()
verificaUsuario listaDeUsuario matriculaProcurado
    | null listaDeUsuario = Just ()
    | matricula (head listaDeUsuario) == matriculaProcurado = Nothing
    | otherwise = verificaUsuario (tail listaDeUsuario) matriculaProcurado