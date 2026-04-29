module Parser(
    parseTransitions,
    parseLine
    ) where

import Types
import Data.Char(isSpace,isUpper,isLower)

-- парсит строки в переходы
parseTransitions :: [String] -> Either String [Transition]
parseTransitions [] = Right []
parseTransitions (x:xs) = do
    a <- parseLine x
    b <- parseTransitions xs
    return (a ++ b)

-- одна строка в список переходов
parseLine :: String -> Either String [Transition]
parseLine line
    | all isSpace line = Right []
    | length ws/=3   = 
          Left "строка должна содержать ровно три элемента"
    | otherwise        = build ws
    where
        ws = words line

-- Строит переход
build :: [String] -> Either String [Transition]
build[a,b,c]
 | length a/=1 || length c/=1=
      Left "Состояния должны быть одной буквой"
 | not(isUpper(head a))=
      Left "Состояние должно быть заглавным"
 | not(isUpper(head c))=
      Left "Состояние должно быть заглавным"
 | b=="eps"= Right [Transition (State a) Eps (State c)]
 | length b/=1=
      Left "Терминал один символ или eps"
 | not(isLower(head b))=
      Left "Терминал должен быть строчным"
 | otherwise= Right [Transition (State a) (Term(head b)) (State c)]

build _ = Left "Неверный формат"