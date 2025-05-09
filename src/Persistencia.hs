module Persistencia where
    
import Tipos
import Funções
import System.IO
import Tipos (Emprestimo)
import System.Directory (doesFileExist)

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
        -- Verifica se o usuario é o mesmo que o usuario do emprestimo
        if usuarioEmprestimo == usuario (head (filter (\x -> livro x == livroEmprestado) listaEmprestimos)) then do
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
            let listaDeEsperaAtualizada = (listaDeEspera (head (filter (\x -> livro x == livroEmprestado) listaEmprestimos))) ++ [usuarioEmprestimo]
            let emprestimoAtualizado = head (filter (\x -> livro x == livroEmprestado) listaEmprestimos)
            let novoEmprestimo = Emprestimo livroEmprestado (usuario emprestimoAtualizado) listaDeEsperaAtualizada
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

listarLivrosIO :: [Livro] -> [Emprestimo] -> IO ()
listarLivrosIO listaLivro emprestimos = do
    putStrLn "Lista de livros disponíveis:"
    listarLista [l | l <- listaLivro, l `notElem` (map livro emprestimos)]
    putStrLn "Lista de livros emprestados:"
    listarLista [l | l <- listaLivro, l `elem` (map livro emprestimos)]

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


listarEmprestadosEDisponiveis :: [Livro] -> [Emprestimo] -> IO ()
listarEmprestadosEDisponiveis listaLivro emprestimos = do
    putStrLn "Livros disponíveis:"
    listarLista [l | l <- listaLivro, l `notElem` (map livro emprestimos)]
    putStrLn "Livros emprestados:"
    listarLista [l | l <- listaLivro, l `elem` (map livro emprestimos)]


historicoEmprestimoUsuario :: [Usuario] -> [Emprestimo] -> IO ()
historicoEmprestimoUsuario listaUsuarios emprestimos = do
    matriculaUsuario <- solicitarEntrada "Digite o número de matricula do usuário:"
    case acharUsuarioMatricula listaUsuarios matriculaUsuario of
        Nothing -> do
            putStrLn "Número de matricula não existe!"
            historicoEmprestimoUsuario listaUsuarios emprestimos
        Just usuarioEncontrado -> do
            let emprestimosUsuario = filter (\x -> matricula (usuario x) == matriculaUsuario) emprestimos
            if null emprestimosUsuario then do
                putStrLn "Nenhum empréstimo encontrado para o usuário."
            else do
                putStrLn "Histórico de empréstimos do usuário:"
                listarLista emprestimosUsuario
-- Exibir livros que apresentem lista de espera n˜ao vazia, juntamente com os usu´arios presentes nesta lista.
listarListaEsperaUsuarios :: [Livro] -> [Usuario] -> [Emprestimo] -> IO ()
listarListaEsperaUsuarios listaLivro listaUsuario emprestimos = do
    putStrLn "Livros com lista de espera:"
    let livrosComListaEspera = filter (\x -> not (null (listaDeEspera x))) emprestimos
    if null livrosComListaEspera then do
        putStrLn "Nenhum livro com lista de espera."
    else do
        mapM_ (\x -> do
            putStrLn ("Livro: " ++ show (livro x))
            putStrLn "Usuários na lista de espera:"
            listarLista (listaDeEspera x)) livrosComListaEspera


editarUsuarioNomeIO :: [Usuario] -> IO [Usuario]
editarUsuarioNomeIO usuarios = do
    putStrLn "Digite a matrícula do usuário:"
    m <- readLn
    case acharUsuarioMatricula usuarios m of
        Nothing -> do
            putStrLn "Usuário não encontrado."
            return usuarios
        Just u -> do
            putStrLn "Digite o novo nome:"
            novoNome <- getLine
            let uAtualizado = u { nome = novoNome }
            return (uAtualizado : removerUsuario usuarios m)

editarUsuarioEmailIO :: [Usuario] -> IO [Usuario]
editarUsuarioEmailIO usuarios = do
    putStrLn "Digite a matrícula do usuário:"
    m <- readLn
    case acharUsuarioMatricula usuarios m of
        Nothing -> do
            putStrLn "Usuário não encontrado."
            return usuarios
        Just u -> do
            putStrLn "Digite o novo email:"
            novoEmail <- getLine
            let uAtualizado = u { email = novoEmail }
            return (uAtualizado : removerUsuario usuarios m)

editarUsuarioMatricIO :: [Usuario] -> IO [Usuario]
editarUsuarioMatricIO usu = do
    putStrLn "Digite a matrícula do usuário:"
    mat <- readLn
    case acharUsuarioMatricula usu mat of
       Nothing -> do
          putStrLn "Usuário não encontrado."
          return usu
       Just u -> do
          putStrLn "Digite a nova matrícula:"
          novMat <- readLn
          case acharUsuarioMatricula usu novMat of
             Nothing -> do
                let matAtualizada = u { matricula = novMat }
                return (matAtualizada : removerUsuario usu mat)
             Just _ -> do
                putStrLn "Matrícula dada já está atribuída a um usuário."
                return usu

editarLivroTituloIO :: [Livro] -> IO [Livro]
editarLivroTituloIO livros = do
    putStrLn "Digite o ID do livro:"
    idLivroEditar <- readLn
    case acharLivroIdentificador livros idLivroEditar of
        Nothing -> do
            putStrLn "Livro não encontrado."
            return livros
        Just l -> do
            putStrLn "Digite o novo título:"
            novoTitulo <- getLine
            let livroAtualizado = l { titulo = novoTitulo }
            return (livroAtualizado : removerLivro livros idLivroEditar)

editarLivroAutorIO :: [Livro] -> IO [Livro]
editarLivroAutorIO livros = do
    putStrLn "Digite o ID do livro:"
    idLivroEditar <- readLn
    case acharLivroIdentificador livros idLivroEditar of
        Nothing -> do
            putStrLn "Livro não encontrado."
            return livros
        Just l -> do
            putStrLn "Digite o novo autor:"
            novoAutor <- getLine
            let livroAtualizado = l { autor = novoAutor }
            return (livroAtualizado : removerLivro livros idLivroEditar)

editarLivroAnoIO :: [Livro] -> IO [Livro]
editarLivroAnoIO livros = do
    putStrLn "Digite o ID do livro:"
    idLivroEditar <- readLn
    case acharLivroIdentificador livros idLivroEditar of
        Nothing -> do
            putStrLn "Livro não encontrado."
            return livros
        Just l -> do
            putStrLn "Digite o novo ano:"
            novoAno <- readLn
            let livroAtualizado = l { ano = novoAno }
            return (livroAtualizado : removerLivro livros idLivroEditar)

editarLivroIDIO:: [Livro] -> IO [Livro]
editarLivroIDIO livros = do
    putStrLn "Digite o ID do Livro:"
    idLivroEdit <- readLn
    case acharLivroIdentificador livros idLivroEdit of
        Nothing -> do
           putStrLn "Livro não encontrado."
           return livros
        Just l -> do
          putStrLn "Digite o novo ID:"
          novoID <- readLn
          case acharLivroIdentificador livros novoID of
             Nothing -> do
                let livroAtualizado = l { idLivro = novoID }
                return (livroAtualizado : removerLivro livros idLivroEdit)
             Just _ -> do
                putStrLn "Já existe um livro com esse ID!"
                return livros

salvarBibliotecaIO :: [Livro] -> [Usuario] -> [Emprestimo] -> IO ()
salvarBibliotecaIO livros usuarios emprestimos = do
    existeArquivo <- doesFileExist "biblioteca.txt"
    if not existeArquivo then do
        putStrLn "O arquivo biblioteca.txt não existe. Deseja criar um novo? (S/N)"
        resposta <- getLine
        case resposta of
            "s" -> do
                arquivo <- openFile "biblioteca.txt" WriteMode
                hPutStrLn arquivo (show livros)
                hPutStrLn arquivo (show usuarios)
                hPutStrLn arquivo (show emprestimos)
                hClose arquivo
                putStrLn "Arquivo criado com sucesso."
            "n" -> putStrLn "Operação cancelada."
            _ -> do
                putStrLn "Opção inválida!"
                salvarBibliotecaIO livros usuarios emprestimos
    else do
        putStrLn "Arquivo biblioteca.txt já existe. Deseja sobrescrevê-lo? (S/N)"
        resposta <- getLine
        case resposta of
            "s" -> do
                arquivo <- openFile "biblioteca.txt" WriteMode
                hPutStrLn arquivo (show livros)
                hPutStrLn arquivo (show usuarios)
                hPutStrLn arquivo (show emprestimos)
                hClose arquivo
                putStrLn "Arquivo sobrescrito com sucesso."
            "n" -> putStrLn "Operação cancelada."
            _ -> do
                putStrLn "Opção inválida!"
                salvarBibliotecaIO livros usuarios emprestimos