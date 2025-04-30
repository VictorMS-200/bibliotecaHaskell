module Persistencia where
    
import Tipos
import Funções

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