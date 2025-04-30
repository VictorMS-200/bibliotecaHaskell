module Tipos where

data Livro = Livro {
    titulo :: String,
    autor :: String,
    ano :: Int,
    idLivro :: Int
} deriving (Show, Eq)

data Usuario = Usuario {
    nome :: String,
    matricula :: Int,
    email :: String
} deriving (Show, Eq)

data Emprestimo = Emprestimo {
    livro :: Livro,
    usuario :: Usuario
} deriving (Show, Eq)