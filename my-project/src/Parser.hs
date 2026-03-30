module Parser(
    parseTransitions,
    parseLine
    ) where

import Types
import Data.Char(isSpace,isUpper,isLower)

-- парсит строки в переходы
parseTransitions :: [String] -> Either String [Transition]
parseTransitions [] = Right []
parseTransitions (line:lines) = do
    ts <- parseLine line
    rest <- parseTransitions lines
    return (ts ++ rest)

-- одна строка в список переходов
parseLine :: String -> Either String [Transition]
parseLine line
    | all isSpace line = Right []
    | length ws /= 3   = Left "строка должна содержать ровно три элемента"
    | otherwise        = build ws
    where
        ws = words line

-- Строит переход
build :: [String] -> Either String [Transition]
build [a,b,c]
    | length a /= 1 || length c /= 1 = Left "состояния должны быть из одной буквы"
    | b == "eps" = Right [Transition (State a) Eps (State c)]
    | length b /= 1 = Left "терминал должен быть одним символом или eps"
    | not (isUpper f) = Left ("состояние " ++ a ++ " должно быть заглавной латинской буквой")
    | not (isUpper t) = Left ("состояние " ++ c ++ " должно быть заглавной латинской буквой")
    | not (isLower s) = Left ("терминал " ++ b ++ " должен быть строчной латинской буквой")
    | otherwise = Right [Transition (State [f]) (Term s) (State [t])]
    where
        f = head a
        s = head b
        t = head c
build _ = Left "неверный формат строки"
