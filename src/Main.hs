import System.IO
import Funções
import Tipos
import Persistencia
import Menu

livrosInicial = [Livro "Titulo1" "autor" 2025 01, Livro "Titulo2" "autor02" 2024 02, Livro "Titulo3" "autor03" 2023 03]
uI = [Usuario "Nome0" 01 "example@email.com", Usuario "Nome1" 02 "example@email.com", Usuario "Nome2" 03 "example@email.com"]
e = [Emprestimo (Livro "Titulo1" "autor" 2025 01) (Usuario "Nome0" 01 "example@email.com") [Usuario "Nome1" 02 "example@email.com", Usuario "Nome2" 03 "example@email.com"]]
e2 = []

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8
    putStrLn "Bem-vindo ao sistema de gerenciamento de biblioteca!"
    laçoMenu livrosInicial uI []