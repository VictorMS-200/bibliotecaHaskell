module Tipos where

data Livro = Livro {
    titulo :: String,
    autor :: String,
    ano :: Int,
    idLivro :: Int
} deriving (Show, Eq)