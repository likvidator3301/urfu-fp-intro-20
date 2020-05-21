module Lecture10.Reader where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Prelude hiding (id)

-- <Задачи для самостоятельного решения>

{-
  Задача: по имеющейся базе данных сфомировать набор рекламных писем.
  Для неженатого(-ой) персоны письмо должно иметь вид:

  Для мужчины:

"""
  Уважаемый Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Для женщины:

"""
  Уважаемая Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Семейным парам шлется одно письмо вида

"""
  Уважаемые Имя_мужа Отчество_мужа и Имя_жены Отчество_жены!
  Разрешите предложить вам наши услуги.
"""

-}

data Sex = Male | Female deriving (Show, Eq, Ord)

type PersonId = Int

data Person = Person
  { id :: Int
  , family :: String
  , name :: String
  , surname :: String
  , sex :: Sex
  , marriedBy :: Maybe Int
  } deriving (Show, Eq, Ord)

persons :: [Person]
persons =
  [ Person 1 "Иванов" "Иван" "Иванович" Male Nothing
  , Person 2 "Петров" "Петр" "Петрович" Male (Just 7)
  , Person 3 "Соловьева" "Алия" "Фаридовна" Female Nothing
  , Person 4 "Кузнецова" "Мария" "Ивановна" Female (Just 8)
  , Person 5 "Гринько" "Юлия" "Владимировна" Female Nothing
  , Person 6 "Кабанов" "Александр" "Романович" Male Nothing
  , Person 7 "Петрова" "Екатерина" "Алексеевна" Female (Just 2)
  , Person 8 "Кузнецов" "Евгений" "Семёнович" Male (Just 4)
  , Person 9 "Антонов" "Юрий" "Васильевич" Male Nothing
  ]

-- Поиск персоны по номеру
findById :: PersonId -> Reader [Person] (Maybe Person)
findById pId = do
  persons' <- ask
  return $ find (\p -> id p == pId) persons'

processSingle :: Person -> String
processSingle (Person _ _ name patronymic sex _) = case sex of
  Male -> "Уважаемый " ++ name ++ " " ++ patronymic ++ "!" ++ "\n" ++ "Разрешите предложить Вам наши услуги."
  Female -> "Уважаемая " ++ name ++ " " ++ patronymic ++ "!" ++ "\n" ++ "Разрешите предложить Вам наши услуги."

processPair :: Person -> Person -> String
processPair (Person _ _ manName manPatronymic _ (Just wifeId)) (Person womanId _ womanName womanPatronymic _ _) =
  if womanId == wifeId then
    "Уважаемые " ++ manName ++ " " ++ manPatronymic ++ " и " ++ womanName ++ " " ++ womanPatronymic ++ "!" ++ "\n" ++ "Разрешите предложить вам наши услуги."
  else
    "Not married"
processPair _ _ = "Not married"

processPerson :: PersonId -> Reader [Person] (Maybe String)
processPerson pId = do
  person <- findById pId
  spouse <- case person of 
    Just (Person _ _ _ _ _ (Just spouseId)) -> findById spouseId
    _ -> return $ Nothing
  return $ case (person, spouse) of 
    (Just man@(Person _ _ _ _ Male _), Just woman@(Person _ _ _ _ Female _)) -> Just (processPair man woman)
    (Just woman@(Person _ _ _ _ Female _), Just man@(Person _ _ _ _ Male _)) -> Just (processPair man woman)
    (Just p, _) -> Just (processSingle p)
    _ -> Nothing

processPersons :: [PersonId] -> [Maybe String]
processPersons personIds = do
  personId <- personIds
  return $ runReader (processPerson personId) persons

-- </Задачи для самостоятельного решения>
