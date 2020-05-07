{-# LANGUAGE DuplicateRecordFields #-}

module Lecture09 where

import System.IO
import System.Directory
import System.Random
import System.FilePath
import Data.List

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show)

newtype Id = Id String deriving (Eq, Show, Read)

newtype Title = Title String deriving (Eq, Show, Read)

newtype Deadline = Deadline String deriving (Eq, Show, Read)

newtype Content = Content String deriving (Eq, Show, Read)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show, Read)

instance Ord Deadline where
  compare (Deadline firstDeadline) (Deadline secondDeadline) = compare firstDeadline secondDeadline

instance Ord Todo where
  compare Todo{deadline=firstDeadline} Todo{deadline=secondDeadline} = compare firstDeadline secondDeadline

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

createInt :: IO Int
createInt = randomIO

createId :: IO Id
createId = fmap (Id . show) createInt

getPath :: TodoList -> Id -> FilePath
getPath (TodoList path) (Id id) = joinPath [path, id]

readFromTodo :: TodoList -> Id -> IO String
readFromTodo todoList id = readFile $ getPath todoList id

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  createDirectory rootFolder
  return $ TodoList rootFolder

isFinished :: Todo -> Bool
isFinished (Todo _ _ _ _ isDone) = isDone

isNotFinished :: Todo -> Bool
isNotFinished = not . isFinished 

printTodos :: [Todo] -> IO ()
printTodos = do
  putStrLn . show

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo todoList title content deadline = do
  id <- createId
  file <- openFile (getPath todoList id) WriteMode
  hPutStr file $ show $ Todo id title content deadline False
  hClose file
  return id

readTodo :: TodoList -> Id -> IO Todo
readTodo todoList id = do
  todo <- readFromTodo todoList id
  return $ read todo

showTodo :: TodoList -> Id -> IO ()
showTodo todoList id = do
  todo <- readFromTodo todoList id
  putStrLn todo

removeTodo :: TodoList -> Id -> IO ()
removeTodo todoList id = removeFile $ getPath todoList id

replaceTodo :: TodoList -> Id -> Todo -> IO ()
replaceTodo todoList id todo = do
  newId <- createId
  file <- openFile (getPath todoList newId) WriteMode
  hPutStr file $ show todo
  hClose file
  let filePath = getPath todoList id
  removeFile filePath
  renameFile (getPath todoList newId) filePath
  

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList id (TodoEdit title content deadline) = do
  todo <- readTodo todoList id
  replaceTodo todoList id (Todo id title content deadline False)

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList id = do
  (Todo id title content deadline _) <- readTodo todoList id
  replaceTodo todoList id (Todo id title content deadline True)

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo (TodoList rootFolder) = do
  files <- listDirectory rootFolder
  todo <- mapM (readTodo (TodoList rootFolder) . Id) files
  return (sort todo)

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
  allTodo <- readAllTodo todoList
  return (filter isNotFinished allTodo)

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = do
  todos <- readAllTodo todoList
  printTodos todos


showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = do
  todos <- readUnfinishedTodo todoList
  printTodos todos

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:
  
  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}

update :: Integer -> IO ()
update x = do
  putStr "Your number: "
  usersNumber <- readLn
  case (compare usersNumber x) of
    LT -> do 
      putStrLn "Too small" 
      update x

    GT -> do 
      putStrLn "Too big" 
      update x

    otherwise -> putStrLn "Yep, that's the number!"

playGuessGame :: IO ()
playGuessGame = do
  putStrLn "Hello! Try to guess the number"
  x <- randomRIO (0, 100)
  update x
  

-- </Задачи для самостоятельного решения>xx