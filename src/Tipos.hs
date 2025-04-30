module Tipos(Livro(..), Usuario(..), Emprestimo(..)) where

data Livro = Livro {
    titulo :: String,
    autor :: String,
    ano :: Int,
    idLivro :: Int
} deriving (Show, Eq, Read)

data Usuario = Usuario {
    nome :: String,
    matricula :: Int,
    email :: String
} deriving (Show, Eq, Read)

data Emprestimo = Emprestimo {
    livro :: Livro,
    usuario :: Usuario
} deriving (Show, Eq, Read)