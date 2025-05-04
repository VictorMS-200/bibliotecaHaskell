module Persistencia where
    
import Tipos
import Funções
import System.IO

adicionarLivroIO :: [Livro] -> IO [Livro]
adicionarLivroIO listaLivro = do
    putStrLn "Digite o identificador do livro:"
    idLivro <- readLn
    case verificaLivro listaLivro idLivro of
        Nothing -> do
            putStrLn "Número de identificação já existe!"
            putStrLn "Tente novamente!"
            adicionarLivroIO listaLivro
        Just _ -> do
            putStrLn "Digite o título do livro:"
            titulo <- getLine
            putStrLn "Digite o autor do livro:"
            autor <- getLine
            putStrLn "Digite o ano do livro:"
            ano <- readLn
            let novoLivro = Livro titulo autor ano idLivro
            return (adicionarLivro novoLivro listaLivro)

removerLivroIO :: [Livro] -> IO [Livro]
removerLivroIO listaLivro = do
    putStrLn "Digite o identificador do livro a ser removido:"
    idLivro <- readLn
    case verificaLivro listaLivro idLivro of
        Just _ -> do
            putStrLn "Número de identificação não existe!"
            putStrLn "Tente novamente!"
            removerLivroIO listaLivro
        Nothing -> do
            return (removerLivro listaLivro idLivro)

adicionarUsuarioIO :: [Usuario] -> IO [Usuario]
adicionarUsuarioIO listaUsuario = do
    putStrLn "Digite o número de matricula do usuário:"
    matriculaUsuario <- readLn
    case verificaUsuario listaUsuario matriculaUsuario of
        Nothing -> do
            putStrLn "Número de matricula já existe!"
            putStrLn "Tente novamente!"
            adicionarUsuarioIO listaUsuario
        Just _ -> do
            putStrLn "Digite o nome do usuário:"
            nome <- getLine
            putStrLn "Digite o email do usuário:"
            email <- getLine
            let novoUsuario = Usuario nome matriculaUsuario email
            return (adicionarUsuario novoUsuario listaUsuario)

removerUsuarioIO :: [Usuario] -> IO [Usuario]
removerUsuarioIO listaUsuario = do
    putStrLn "Digite o número de matricula do usuário a ser removido:"
    matriculaUsuario <- readLn
    case verificaUsuario listaUsuario matriculaUsuario of
        Just _ -> do
            putStrLn "Número de matricula não existe!"
            putStrLn "Tente novamente!"
            removerUsuarioIO listaUsuario
        Nothing -> do
            return (removerUsuario listaUsuario matriculaUsuario)