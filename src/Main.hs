import System.IO
import Funções
import Tipos
import Persistencia
import Menu

livrosIniciais = [Livro "Titulo1" "Autor1" 2025 01, Livro "Titulo2" "Autor2" 2024 02, Livro "Titulo3" "Autor3" 2023 03]
usuariosIniciais = [Usuario "Nome1" 01 "exemplo@email.com", Usuario "Nome2" 02 "exemplo@email.com"]
emprestimosIniciais = [Emprestimo (Livro "Titulo1" "Autor1" 2025 01) (Usuario "Nome1" 01 "exemplo@email.com") []]


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8
    putStrLn "Bem-vindo ao sistema de gerenciamento de biblioteca!"
    laçoMenu livrosIniciais usuariosIniciais emprestimosIniciais