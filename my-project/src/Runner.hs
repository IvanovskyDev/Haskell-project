module Runner (run) where

import Parser (parseTransitions)
import Grammar (rightGrammar, leftGrammar)
import Determinization (determinize, isDetermin)
import Output (formatGrammar, formDKA)
import AutomatShow (viewNKA, viewDKA)
import Validate (validateNKA)
import Types

import Data.List (nub)


run :: IO ()
run = do
    putStrLn "Введите имя файла:"
    path <- getLine

    content <- readFile path

    case parseTransitions (lines content) of
        Left err -> do
            putStrLn ("Ошибка парсинга: " ++ err)

        Right trans -> do
            runPipeline path trans


runPipeline :: String -> [Transition] -> IO ()
runPipeline inputFile trans = do

    let nka = buildNKA trans

    -- Валидация автомата
    case validateNKA nka of
        Left errs -> do
            putStrLn "Ошибки автомата:"
            mapM_ putStrLn errs

        Right _ -> do
            processValidAutomaton inputFile nka


-- ОБРАБОТКА ВАЛИДНОГО АВТОМАТА

processValidAutomaton :: String -> NKA -> IO ()
processValidAutomaton inputFile nka = do

    putStrLn "\nВыберите тип грамматики:"
    putStrLn "0 - праволинейная"
    putStrLn "1 - леволинейная"

    typ <- getLine

    let grammar = selectGrammar nka typ

    putStrLn "\nРегулярная грамматика "
    putStrLn (formatGrammar grammar)

    -- Проверка детерминизма
    if isDetermin nka
        then handleDeterministic inputFile nka grammar
        else handleNonDeterministic inputFile nka grammar


-- ДЕТЕРМИНИРОВАННЫЙ АВТОМАТ
handleDeterministic :: String -> NKA -> Grammar -> IO ()
handleDeterministic inputFile nka grammar = do

    putStrLn "\nАвтомат уже детерминирован"

    let outFile = makeOutputFile inputFile

    writeFile outFile (formatGrammar grammar)

    putStrLn ("Результат записан в " ++ outFile)

    viewNKA nka



-- НЕДЕТЕРМИНИРОВАННЫЙ АВТОМАТ
handleNonDeterministic :: String -> NKA -> Grammar -> IO ()
handleNonDeterministic inputFile nka grammar = do

    let dka = determinize nka
    let outFile = makeOutputFile inputFile

    putStrLn "\nДетерминированный автомат"
    putStrLn (formDKA dka)

    writeFile
        outFile
        ( formatGrammar grammar
          ++ "\n\n"
          ++ formDKA dka )

    putStrLn ("Результат записан в " ++ outFile)

    viewDKA dka


-- ВЫБОР ГРАММАТИКИ
selectGrammar :: NKA -> String -> Grammar
selectGrammar nka typ =
    case typ of
        "1" -> leftGrammar nka
        _   -> rightGrammar nka



-- ПОСТРОЕНИЕ НКА
buildNKA :: [Transition] -> NKA
buildNKA trans =
    NKA
        { nkaStates      = states
        , nkaTerminals   = alphabet
        , nkaTransitions = map convert trans
        , nkaStart       = State "H"
        , nkaFinal       = [State "S"]
        }

  where
    states =
        nub (concatMap (\t -> [from t, to t]) trans
             ++ [State "H", State "S"])

    alphabet =
        nub [c | Transition _ (Term c) _ <- trans]

    convert t =
        (from t, onTerm t, to t)


makeOutputFile :: String -> String
makeOutputFile input =
    let (name, _) = break (== '.') input
    in name ++ "_output.txt"