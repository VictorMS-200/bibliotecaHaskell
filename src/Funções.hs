module Funções where

import Tipos

acharLivroIdentificador :: [Livro] -> Int -> Maybe Livro
acharLivroIdentificador [] _ = Nothing
acharLivroIdentificador listaDeLivros matriculaProcurada
    | null listaDeLivros = Nothing
    | idLivro (head listaDeLivros) == matriculaProcurada = Just (head listaDeLivros)
    | otherwise = acharLivroIdentificador (tail listaDeLivros) matriculaProcurada

acharUsuarioMatricula :: [Usuario] -> Int -> Maybe Usuario
acharUsuarioMatricula [] _ = Nothing
acharUsuarioMatricula listaDeUsuarios matriculaProcurada
    | null listaDeUsuarios = Nothing
    | matricula (head listaDeUsuarios) == matriculaProcurada = Just (head listaDeUsuarios)
    | otherwise = acharUsuarioMatricula (tail listaDeUsuarios) matriculaProcurada

adicionarData :: a -> [a] -> [a]
adicionarData novoElemento listaDeEspera = novoElemento : listaDeEspera

removerLivro :: [Livro] -> Int -> [Livro]
removerLivro listaDeLivro idProcurado
    | null listaDeLivro = []
    | idLivro (head listaDeLivro) == idProcurado = tail listaDeLivro
    | otherwise = head listaDeLivro : removerLivro (tail listaDeLivro) idProcurado

removerEmprestimo :: [Emprestimo] -> Livro -> [Emprestimo]
removerEmprestimo listaDeEmprestimos livroProcurado
    | null listaDeEmprestimos = []
    | livro (head listaDeEmprestimos) == livroProcurado = tail listaDeEmprestimos
    | otherwise = head listaDeEmprestimos : removerEmprestimo (tail listaDeEmprestimos) livroProcurado

verificaLivro :: [Livro] -> Int -> Maybe ()
verificaLivro listaDeLivro idProcurado
    | null listaDeLivro = Just ()
    | idLivro (head listaDeLivro) == idProcurado = Nothing
    | otherwise = verificaLivro (tail listaDeLivro) idProcurado

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

registrarEmprestimo :: Livro -> Usuario -> [Emprestimo] -> [Emprestimo]
registrarEmprestimo livro usuario listaDeEmprestimos = Emprestimo livro usuario [] : listaDeEmprestimos

registrarDevolucao :: Livro -> Usuario -> [Emprestimo] -> [Emprestimo]
registrarDevolucao livro usuario listaDeEmprestimos = Emprestimo livro usuario [] : removerEmprestimo listaDeEmprestimos livro

