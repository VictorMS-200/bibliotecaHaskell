module Funções where

import Tipos

acharLivroMatricula :: [Livro] -> Int -> Maybe Livro
acharLivroMatricula [] _ = Nothing
acharLivroMatricula listaDeLivros matriculaProcurada
    | null listaDeLivros = Nothing
    | idLivro (head listaDeLivros) == matriculaProcurada = Just (head listaDeLivros)
    | otherwise = acharLivroMatricula (tail listaDeLivros) matriculaProcurada

acharUsuarioMatricula :: [Usuario] -> Int -> Maybe Usuario
acharUsuarioMatricula [] _ = Nothing
acharUsuarioMatricula listaDeUsuarios matriculaProcurada
    | null listaDeUsuarios = Nothing
    | matricula (head listaDeUsuarios) == matriculaProcurada = Just (head listaDeUsuarios)
    | otherwise = acharUsuarioMatricula (tail listaDeUsuarios) matriculaProcurada

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

removerUsuario :: [Usuario] -> Int -> [Usuario]
removerUsuario listaDeUsuario matriculaProcurada
    | null listaDeUsuario = []
    | matricula (head listaDeUsuario) == matriculaProcurada = tail listaDeUsuario
    | otherwise = head listaDeUsuario : removerUsuario (tail listaDeUsuario) matriculaProcurada

verificaUsuario :: [Usuario] -> Int -> Maybe ()
verificaUsuario listaDeUsuario matriculaProcurado
    | null listaDeUsuario = Just ()
    | matricula (head listaDeUsuario) == matriculaProcurado = Nothing
    | otherwise = verificaUsuario (tail listaDeUsuario) matriculaProcurado

listarLista :: Show a => [a] -> IO ()
listarLista [] = putStrLn "Lista vazia!"
listarLista lista = do
    putStrLn "Lista:"
    mapM_ (putStrLn . show) lista

registrarEmprestimo :: Livro -> Usuário -> [Empréstimo] -> [Empréstimo]
registrarEmprestimo livro usuario listaDeEmprestimos = Emprestimo livro usuario : listaDeEmprestimos
