module Persistencia where
    
import Tipos
import Funções
import System.IO

livrosInicial = [Livro "Titulo1" "autor" 2025 01, Livro "Titulo2" "autor02" 2024 02, Livro "Titulo3" "autor03" 2023 03]
uI = [Usuario "Nome0" 01 "example@email.com", Usuario "Nome1" 02 "example@email.com", Usuario "Nome2" 03 "example@email.com"]
e = [Emprestimo (Livro "Titulo1" "autor" 2025 01) (Usuario "Nome0" 01 "example@email.com") [Usuario "Nome1" 02 "example@email.com", Usuario "Nome2" 03 "example@email.com"]]

solicitarEntrada :: Read a => String -> IO a
solicitarEntrada mensagem = do
    putStrLn mensagem
    readLn

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
            return (adicionarData novoLivro listaLivro)

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
    matriculaUsuario <- solicitarEntrada "Digite o número de matricula do usuário:"
    case verificaUsuario listaUsuario matriculaUsuario of
        Nothing -> do
            putStrLn "Número de matricula já existe!"
            adicionarUsuarioIO listaUsuario
        Just _ -> do
            putStrLn "Digite o nome do usuário:"
            nome <- getLine
            putStrLn "Digite o email do usuário:"
            email <- getLine
            let novoUsuario = Usuario nome matriculaUsuario email
            return (adicionarData novoUsuario listaUsuario)

removerUsuarioIO :: [Usuario] -> IO [Usuario]
removerUsuarioIO listaUsuario = do
    matriculaUsuario <- solicitarEntrada "Digite o número de matricula do usuário a ser removido:"
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

registrarDevolucaoIO :: [Livro] -> [Usuario] -> [Emprestimo] -> IO [Emprestimo]
registrarDevolucaoIO listaLivro listaUsuario listaEmprestimos = do
    hSetBuffering stdout LineBuffering
    hSetEncoding stdout utf8
    matriculaUsuario <- solicitarEntrada "Digite o número de matricula do usuário:"
    case acharUsuarioMatricula listaUsuario matriculaUsuario of
        Nothing -> do
            putStrLn "Número de matricula não existe!"
            registrarDevolucaoIO listaLivro listaUsuario listaEmprestimos
        Just usuarioDevolucao -> do
            idLivro <- solicitarEntrada "Digite o identificador do livro:"
            case acharLivroIdentificador listaLivro idLivro of
                Nothing -> do
                    putStrLn "Número de identificação não existe!"
                    registrarDevolucaoIO listaLivro listaUsuario listaEmprestimos
                Just livroDevolvido -> processarDevolucao livroDevolvido usuarioDevolucao listaEmprestimos

processarDevolucao :: Livro -> Usuario -> [Emprestimo] -> IO [Emprestimo]
processarDevolucao livroDevolvido usuarioDevolucao listaEmprestimos = do
    let emprestimoExistente = filter (\x -> livro x == livroDevolvido && usuario x == usuarioDevolucao) listaEmprestimos
    if null emprestimoExistente then do
        putStrLn "Empréstimo não encontrado!"
        return listaEmprestimos
    else do
        let novoEmprestimo = head emprestimoExistente
        if listaDeEspera novoEmprestimo == [] then do
            putStrLn "Devolução registrada com sucesso!"
            return (removerEmprestimo listaEmprestimos livroDevolvido)
        else do
            let usuarioPrimeiroListaEspera = head (listaDeEspera novoEmprestimo)
            putStrLn ("Usuário " ++ nome usuarioPrimeiroListaEspera ++ " foi colocado para empréstimo.")
            let listaDeEsperaAtualizada = tail (listaDeEspera novoEmprestimo)
            let novoEmprestimoAtualizado = Emprestimo { livro = livroDevolvido, usuario = usuarioPrimeiroListaEspera, listaDeEspera = listaDeEsperaAtualizada }
            putStrLn "Devolução registrada com sucesso!"
            return (novoEmprestimoAtualizado : removerEmprestimo listaEmprestimos livroDevolvido)

listarLivrosIO :: [Livro] -> IO ()
listarLivrosIO listaLivro = do
    putStrLn "Lista de livros disponíveis:"
    listarLista [l | l <- listaLivro, l `notElem` (map livro e)]
    putStrLn "Lista de livros emprestados:"
    listarLista [l | l <- listaLivro, l `elem` (map livro e)]

listarListasEsperaIO :: [Emprestimo] -> IO ()
listarListasEsperaIO listaEmprestimos = do
    livroListar <- solicitarEntrada "Digite o identificador do livro:"
    case acharLivroIdentificador (map livro listaEmprestimos) livroListar of
        Nothing -> do
            putStrLn "Livro não se encontrar na lista de empréstimos ou não existe!"
            listarListasEsperaIO listaEmprestimos
        Just livroEncontrado -> do
            let emprestimoEncontrado = filter (\x -> livro x == livroEncontrado) listaEmprestimos
            if null emprestimoEncontrado then do
                putStrLn "Nenhum usuário na lista de espera!"
                return ()
            else do
                putStrLn "Lista de espera:"
                listarLista (listaDeEspera (head emprestimoEncontrado))

