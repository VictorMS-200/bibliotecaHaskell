import Text.XHtml (menu)
exibirOpcoes :: [String] -> IO ()
exibirOpcoes opcoes = do
    putStrLn "\nEscolha uma opção:\n"
    putStrLn (unlines opcoes)
    putStrLn ""

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

laçoMenu :: IO ()
laçoMenu = do
    opcao <- menuPrincipal
    case opcao of
        1 -> do
            opcaoLivros <- menuLivros
            laçoMenu
        2 -> do
            opcaoUsuarios <- menuUsuarios
            laçoMenu
        3 -> do
            opcaoEmprestimos <- menuEmprestimos
            laçoMenu
        4 -> do
            opcaoRelatorios <- menuRelatorios
            laçoMenu
        5 -> do
            opcaoEditarLivro <- menuEditarLivro
            laçoMenu
        6 -> do
            opcaoEditarUsuario <- menuEditarUsuario
            laçoMenu
        7 -> putStrLn "Saindo..."
        _ -> putStrLn "Opção inválida. Tente novamente."