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
         "\n  получено:  " ++ a) : compareLines as es


isGrammarRule :: String -> Bool
isGrammarRule s = '=' `elem` s && ';' `elem` s

isDkaTransition :: String -> Bool
isDkaTransition s = "--" `isInfixOf` s && "-->" `isInfixOf` s


compareGrammarRule :: String -> String -> [String]
compareGrammarRule a e =
    let (al, ar) = parseRule a
        (el, er) = parseRule e

        aAlts = sort (splitOn "|" ar)
        eAlts = sort (splitOn "|" er)

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
        (l, '=':r) -> (l, trim (initSafe r))
        _          -> ("", "")


initSafe :: String -> String
initSafe [] = []
initSafe xs = xs


trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')


compareDkaTransition :: String -> String -> [String]
compareDkaTransition a e
    | a == e = []
    | otherwise =
        ["Несовпадение перехода ДКА:\n  ожидалось: " ++ e ++
         "\n  получено:  " ++ a]


splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delim xs = go xs
  where
    go [] = [[]]
    go s =
        let (pre, rest) = breakList delim s
        in pre : case rest of
            [] -> []
            _  -> go (drop (length delim) rest)


breakList :: Eq a => [a] -> [a] -> ([a], [a])
breakList _ [] = ([], [])
breakList delim xs@(y:ys)
    | delim `isPrefixOf` xs = ([], xs)
    | otherwise =
        let (p, r) = breakList delim ys
        in (y:p, r)


isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys