# bibliotecaHaskell

## Descrição
Este projeto é uma biblioteca de gerenciamento de livros em Haskell. Ele permite que os usuários adicionem, removam e listem livros, além de verificar se um livro está disponível ou não. A biblioteca é projetada para ser simples e fácil de usar, com uma interface de linha de comando para interação.

O projeto foi desenvolvido como parte de um trabalho da matéria de programação funcional da Universidade Federal de Uberlândia (UFU).

# Como usar
Para compilar e executar o projeto, primeiro, você precisa ter o [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/downloads/) instalado.

Após instalar o GHC, você pode clonar o repositório do projeto usando o seguinte comando:

```bash	
git clone https://github.com/VictorMS-2003/bibliotecaHaskell.git
```

Navegue até o diretório `src`:

```bash
cd bibliotecaHaskell/src
```

Então, você pode compilar o projeto com o seguinte comando:

```bash
ghc -o bibliotecaHaskell .\Main.hs 
```

Depois de compilar, você pode executar o programa com o seguinte comando:

```bash
.\bibliotecaHaskell
```

#### Obs: 
- o nome {bibliotecaHaskell} pode ser qualquer nome que você preferir, desde que não tenha espaços ou caracteres especiais.
- O arquivo Main.hs é o ponto de entrada do programa. Você pode modificar o código dentro dele para alterar o comportamento do programa.
- Se você estiver usando linux ou macOS, o comando para executar o programa pode ser ligeiramente diferente. Você pode usar `./bibliotecaHaskell` em vez de `.\bibliotecaHaskell`.
