module Persistencia where
    
import Tipos
import Funções
import System.IO
import Distribution.Simple.Program.HcPkg (list)

livrosInicial = [Livro "Titulo1" "autor" 2025 01, Livro "Titulo2" "autor02" 2024 02]
uI = [Usuario "Nome0" 01 "example@email.com", Usuario "Nome1" 02 "example@email.com"]
e = [Emprestimo (Livro "Titulo1" "autor" 2025 01) (Usuario "Nome0" 01 "example@email.com") []]

solicitarEntrada :: Read a => String -> IO a
solicitarEntrada mensagem = do
    putStrLn mensagem
    readLn

adicionarLivroIO :: [Livro] -> IO [Livro]
adicionarLivroIO listaLivro = do
    idLivro <- solicitarEntrada "Digite o identificador do livro:"
    case verificaLivro listaLivro idLivro of
        Nothing -> do
            putStrLn "Número de identificação já existe!"
            putStrLn "Digite um novo número de identificação!"
            adicionarLivroIO listaLivro
        Just _ -> do
            titulo <- solicitarEntrada "Digite o título do livro:"
            autor <- solicitarEntrada "Digite o autor do livro:"
            ano <- solicitarEntrada "Digite o ano do livro:"
            let novoLivro = Livro titulo autor ano idLivro
            return (adicionarData novoLivro listaLivro)

removerLivroIO :: [Livro] -> IO [Livro]
removerLivroIO listaLivro = do
    idLivro <- solicitarEntrada "Digite o identificador do livro:"
    case verificaLivro listaLivro idLivro of
        Just _ -> do
            putStrLn "Número de identificação não existe!"
            putStrLn "Digite um novo número de identificação!"
            removerLivroIO listaLivro
        Nothing -> do
            return (removerLivro listaLivro idLivro)

adicionarUsuarioIO :: [Usuario] -> IO [Usuario]
adicionarUsuarioIO listaUsuario = do
    matriculaUsuario <- solicitarEntrada "Digite o número de matricula do usuário:"
    case verificaUsuario listaUsuario matriculaUsuario of
        Nothing -> do
            putStrLn "Número de matricula já existe!"
            adicionarUsuarioIO listaUsuario
        Just _ -> do
            nome <- solicitarEntrada "Digite o nome do usuário:"
            putStrLn "Digite o email do usuário:"
            email <- solicitarEntrada "Digite o email do usuário:"
            let novoUsuario = Usuario nome matriculaUsuario email
            return (adicionarData novoUsuario listaUsuario)

removerUsuarioIO :: [Usuario] -> IO [Usuario]
removerUsuarioIO listaUsuario = do
    putStrLn "Digite o número de matricula do usuário a ser removido:"
    matriculaUsuario <- readLn
    case verificaUsuario listaUsuario matriculaUsuario of
        Just _ -> do
            putStrLn "Número de matricula não existe!"
            removerUsuarioIO listaUsuario
        Nothing -> do
            return (removerUsuario listaUsuario matriculaUsuario)

registrarEmprestimoIO :: [Livro] -> [Usuario] -> [Emprestimo] -> IO [Emprestimo]
registrarEmprestimoIO listaLivro listaUsuario listaEmprestimos = do
    matriculaUsuario <- solicitarEntrada "Digite o número de matricula do usuário:"
    case acharUsuarioMatricula listaUsuario matriculaUsuario of
        Nothing -> do
            putStrLn "Número de matricula não existe!"
            registrarEmprestimoIO listaLivro listaUsuario listaEmprestimos
        Just usuarioEmprestimo -> do
            idLivro <- solicitarEntrada "Digite o identificador do livro:"
            case acharLivroIdentificador listaLivro idLivro of
                Nothing -> do
                    putStrLn "Número de identificação não existe!"
                    registrarEmprestimoIO listaLivro listaUsuario listaEmprestimos
                Just livroEmprestado -> processarEmprestimo livroEmprestado usuarioEmprestimo listaEmprestimos

processarEmprestimo :: Livro -> Usuario -> [Emprestimo] -> IO [Emprestimo]
processarEmprestimo livroEmprestado usuarioEmprestimo listaEmprestimos =
    if elem livroEmprestado (map livro listaEmprestimos) then
        if elem usuarioEmprestimo (map usuario listaEmprestimos) then do
            putStrLn "Usuário já possui o livro emprestado!"
            return listaEmprestimos
        else gerenciarListaDeEspera livroEmprestado usuarioEmprestimo listaEmprestimos
    else do
        let novoEmprestimo = Emprestimo livroEmprestado usuarioEmprestimo []
        return (novoEmprestimo : listaEmprestimos)

gerenciarListaDeEspera :: Livro -> Usuario -> [Emprestimo] -> IO [Emprestimo]
gerenciarListaDeEspera livroEmprestado usuarioEmprestimo listaEmprestimos = do
    putStrLn "Livro se encontra emprestado!"
    putStrLn "Deseja adicionar o usuário a lista de espera? (s/n)"
    resposta <- getLine
    case resposta of
        "s" -> do
            let listaDeEsperaAtualizada = adicionarData usuarioEmprestimo (listaDeEspera (head (filter (\x -> livro x == livroEmprestado) listaEmprestimos)))
            let novoEmprestimo = Emprestimo livroEmprestado usuarioEmprestimo listaDeEsperaAtualizada
            return (novoEmprestimo : removerEmprestimo listaEmprestimos livroEmprestado)
        "n" -> do
            putStrLn "Usuário não foi adicionado a lista de espera!"
            return listaEmprestimos
        _ -> do
            putStrLn "Opção inválida!"
            gerenciarListaDeEspera livroEmprestado usuarioEmprestimo listaEmprestimos

