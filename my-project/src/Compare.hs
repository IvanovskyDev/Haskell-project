module Compare (compareOutputs) where

import Data.List (sort, intercalate, isInfixOf)


compareOutputs :: String -> String -> [String]
compareOutputs actual expected =
    let a = filter (not . null) (lines actual)
        e = filter (not . null) (lines expected)
    in compareLines a e


compareLines :: [String] -> [String] -> [String]
compareLines [] [] = []

compareLines [] (e:es) =
    ("Нет строки: " ++ e) : compareLines [] es

compareLines (a:as) [] =
    ("Лишняя строка: " ++ a) : compareLines as []

compareLines (a:as) (e:es)
    | a == e = compareLines as es

    | isGrammarRule a && isGrammarRule e =
        compareGrammarRule a e ++ compareLines as es

    | isDkaTransition a && isDkaTransition e =
        compareDkaTransition a e ++ compareLines as es

    | otherwise =
        ("Несовпадение:\n  ожидалось: " ++ e ++
         "\n  получено:  " ++ a)
        : compareLines as es



isGrammarRule :: String -> Bool
isGrammarRule s = '=' `elem` s && ';' `elem` s

isDkaTransition :: String -> Bool
isDkaTransition s = "--" `isInfixOf` s && "-->" `isInfixOf` s




compareGrammarRule :: String -> String -> [String]
compareGrammarRule a e =
    let (al, ar) = parseRule a
        (el, er) = parseRule e

        aAlts = sort (splitOn '|' ar)
        eAlts = sort (splitOn '|' er)

        err1 =
            if al /= el
                then ["Разные нетерминалы: " ++ el ++ " vs " ++ al]
                else []

        err2 =
            if aAlts /= eAlts
                then ["Разные альтернативы для " ++ el ++
                      "\nожидалось: " ++ intercalate "|" eAlts ++
                      "\nполучено:  " ++ intercalate "|" aAlts]
                else []

    in err1 ++ err2


parseRule :: String -> (String, String)
parseRule s =
    case break (== '=') s of
        (l, '=':r) -> (trim l, trim r)
        _          -> ("", "")




compareDkaTransition :: String -> String -> [String]
compareDkaTransition a e
    | a == e = []
    | otherwise =
        ["Несовпадение перехода ДКА:\n  ожидалось: " ++ e ++
         "\n  получено:  " ++ a]



splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s =
    let (p, r) = break (== c) s
    in p : case r of
        []      -> []
        (_:rs)  -> splitOn c rs


trim :: String -> String
trim = reverse . dropWhile (== ' ')
     . reverse . dropWhile (== ' ')