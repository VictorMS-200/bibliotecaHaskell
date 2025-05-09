module Menu where
import Tipos
import Funções
import Persistencia
import System.IO
import Control.Monad.RWS.Class (MonadState(put))


exibirOpcoes :: [String] -> IO ()
exibirOpcoes opcoes = do
    putStrLn "========================="
    putStrLn "Escolha uma opção:"
    putStrLn "========================="
    putStrLn (unlines opcoes)

lerOpcao :: Int -> Int -> IO Int
lerOpcao min max = do
    opcao <- readLn :: IO Int
    valida opcao
    where
        valida :: Int -> IO Int
        valida opcao
            | min <= opcao && opcao <= max = return opcao
            | otherwise = do
                putStrLn $ "Opção inválida. Digite um número entre " ++ show min ++ " e " ++ show max ++ "."
                opcao <- readLn :: IO Int
                valida opcao

menuPrincipal :: IO Int
menuPrincipal = do
    let opcoes = ["1 - Cadastrar Livros",
                  "2 - Cadastrar Usuários",
                  "3 - Empréstimo e Devolução",
                  "4 - Relatórios",
                  "5 - Editar Livro",
                  "6 - Editar Usuário",
                  "7 - Salvar e Sair"]
    exibirOpcoes opcoes
    lerOpcao 1 7

menuLivros :: IO Int
menuLivros = do
    let opcoes = ["1 - Cadastrar Livro",
                  "2 - Listar Livros",
                  "3 - Remover Livro",
                  "4 - Voltar"]
    exibirOpcoes opcoes
    lerOpcao 1 4

menuUsuarios :: IO Int
menuUsuarios = do
    let opcoes = ["1 - Cadastrar Usuário",
                  "2 - Listar Usuários",
                  "3 - Remover Usuário",
                  "4 - Voltar"]
    exibirOpcoes opcoes
    lerOpcao 1 4

menuEmprestimos :: IO Int
menuEmprestimos = do
    let opcoes = ["1 - Realizar Empréstimo",
                  "2 - Realizar Devolução",
                  "3 - Listar Empréstimos",
                  "4 - Listar Espera",
                  "5 - Voltar"]
    exibirOpcoes opcoes
    lerOpcao 1 5

menuRelatorios :: IO Int
menuRelatorios = do
    let opcoes = ["1 - Relatório de Empréstimos",
                  "2 - Histórico de Empréstimos do Usuário",
                  "3 - Livros com lista de espera",
                  "4 - Voltar"]
    exibirOpcoes opcoes
    lerOpcao 1 4

menuEditarLivro :: IO Int
menuEditarLivro = do
    let opcoes = ["1 - Editar ID do Livro",
                  "2 - Editar Título do Livro",
                  "3 - Editar Autor do Livro",
                  "4 - Editar Ano do Livro",
                  "5 - Voltar"]
    exibirOpcoes opcoes
    lerOpcao 1 5

menuEditarUsuario :: IO Int
menuEditarUsuario = do
    let opcoes = ["1 - Editar matrícula do Usuário",
                  "2 - Editar Nome do Usuário",
                  "3 - Editar Email do Usuário",
                  "4 - Voltar"]
    exibirOpcoes opcoes
    lerOpcao 1 4

laçoMenu :: [Livro] -> [Usuario] -> [Emprestimo] -> IO ()
laçoMenu livros usuarios emprestimos = do
    opcao <- menuPrincipal
    case opcao of
        1 -> do
            livrosAtualizados <- laçoMenuLivros livros emprestimos
            laçoMenu livrosAtualizados usuarios emprestimos
        2 -> do
            usuariosAtualizados <- laçoMenuUsuarios usuarios
            laçoMenu livros usuariosAtualizados emprestimos
        3 -> do
            emprestimosAtualizados <- laçoMenuEmprestimos livros usuarios emprestimos
            laçoMenu livros usuarios emprestimosAtualizados
        4 -> do
            opcaoRelatorios <- menuRelatorios
            laçoMenu livros usuarios emprestimos
        5 -> do
            opcaoEditarLivro <- menuEditarLivro
            laçoMenu livros usuarios emprestimos
        6 -> do
            opcaoEditarUsuario <- menuEditarUsuario
            laçoMenu livros usuarios emprestimos
        7 -> putStrLn "Saindo..."
        _ -> putStrLn "Opção inválida. Tente novamente."


laçoMenuLivros :: [Livro] -> [Emprestimo] -> IO [Livro]
laçoMenuLivros livros emprestimos = do
    opcao <- menuLivros
    case opcao of
        1 -> do
            putStrLn "Adicionar livro:"
            adicionarLivroIO livros 
        2 -> do
            putStrLn "Listar livros:"
            listarLivrosIO livros emprestimos
            return livros
        3 -> do
            putStrLn "Remover livro:"
            removerLivroIO livros
        4 -> do
            putStrLn "Voltando..."
            return livros

laçoMenuUsuarios :: [Usuario] -> IO [Usuario]
laçoMenuUsuarios usuarios = do
    opcao <- menuUsuarios
    case opcao of
        1 -> do
            putStrLn "Adicionar usuário:"
            adicionarUsuarioIO usuarios 
        2 -> do
            putStrLn "Listar usuários:"
            listarLista usuarios
            return usuarios
        3 -> do
            putStrLn "Remover usuário:"
            removerUsuarioIO usuarios
        4 -> do
            putStrLn "Voltando..."
            return usuarios

laçoMenuEmprestimos :: [Livro] -> [Usuario] -> [Emprestimo] -> IO [Emprestimo]
laçoMenuEmprestimos livros usuarios emprestimos = do
    opcao <- menuEmprestimos
    case opcao of
        1 -> do
            putStrLn "Realizar empréstimo:"
            emprestimosAtualizados <- registrarEmprestimoIO livros usuarios emprestimos
            return emprestimosAtualizados
        2 -> do
            putStrLn "Realizar devolução:"
            emprestimosAtualizados <- registrarDevolucaoIO livros usuarios emprestimos
            return emprestimosAtualizados
        3 -> do
            putStrLn "Listar empréstimos:"
            listarLista emprestimos
            return emprestimos
        4 -> do
            putStrLn "Listar espera:"
            return emprestimos
        5 -> do
            putStrLn "Voltando..."
            return emprestimos