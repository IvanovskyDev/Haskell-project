module Runner (run) where

import Parser (parseTransitions)
import Grammar (rightGrammar, leftGrammar)
import Determinization (determinize, isDetermin)
import Output (formatGrammar, formDKA)
import AutomatShow (viewNKA, viewDKA)
import Validate (validateNKA)
import Compare (compareOutputs)
import Types

import Data.List (nub)
import Control.Exception (try, IOException)
--import System.IO (readFile)


run :: IO ()
run = do
    putStrLn "Введите имя файла:"
    path <- getLine

    contentResult <- safeReadFile path

    case contentResult of
        Left err -> putStrLn err

        Right content ->
            case parseTransitions (lines content) of

                Left err ->
                    putStrLn ("Ошибка парсинга: " ++ err)

                Right trans ->
                    runPipeline path trans



safeReadFile :: String -> IO (Either String String)
safeReadFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _  -> return (Left "Ошибка: файл не существует")
        Right c -> return (Right c)



runPipeline :: String -> [Transition] -> IO ()
runPipeline inputFile trans = do

    let nka = buildNKA trans

    if null (nkaTransitions nka)
        then putStrLn "Ошибка: автомат не содержит переходов"
        else do
            case validateNKA nka of

                Left errs -> do
                    putStrLn "Ошибки автомата:"
                    mapM_ putStrLn errs

                Right _ ->
                    processAutomaton inputFile nka



processAutomaton :: String -> NKA -> IO ()
processAutomaton inputFile nka = do

    putStrLn "\nВыберите тип грамматики:"
    putStrLn "0 - праволинейная"
    putStrLn "1 - леволинейная"

    typ <- getLine

    case typ of
        "0" -> runGrammar inputFile nka False
        "1" -> runGrammar inputFile nka True
        _   -> putStrLn "Ошибка: нужно 0 или 1"



runGrammar :: String -> NKA -> Bool -> IO ()
runGrammar inputFile nka isLeft = do

    let grammar =
            if isLeft
                then leftGrammar nka
                else rightGrammar nka

    putStrLn "\nРегулярная грамматика"
    putStrLn (formatGrammar grammar)

    let mode = if isLeft then 1 else 0

    let outFile = makeOutputFile inputFile mode
    let expFile = makeExceptionFile inputFile mode

    if isDetermin nka
        then do
            deterministic nka grammar outFile expFile
        else do
            nonDeterministic nka grammar outFile expFile



deterministic :: NKA -> Grammar -> String -> String -> IO ()
deterministic nka grammar outFile expFile = do

    putStrLn "\nАвтомат уже детерминирован"

    writeFile outFile (formatGrammar grammar)

    putStrLn ("Результат записан в " ++ outFile)
    putStrLn ("Ожидаемый результат: " ++ expFile)

    viewNKA nka

    compareFiles outFile expFile



nonDeterministic :: NKA -> Grammar -> String -> String -> IO ()
nonDeterministic nka grammar outFile expFile = do

    let dka = determinize nka

    putStrLn "\nДетерминированный автомат"
    putStrLn (formDKA dka)

    writeFile outFile
        ( formatGrammar grammar
          ++ "\n\n"
          ++ formDKA dka )

    putStrLn ("Результат записан в " ++ outFile)
    putStrLn ("Ожидаемый результат: " ++ expFile)

    viewDKA dka

    compareFiles outFile expFile


compareFiles :: String -> String -> IO ()
compareFiles actualFile expectedFile = do
    actualResult <- try (readFile actualFile) :: IO (Either IOException String)
    expectedResult <- try (readFile expectedFile) :: IO (Either IOException String)

    case (actualResult, expectedResult) of

        (Right actual, Right expected) -> do
            let diff = compareOutputs actual expected

            if null diff
                then putStrLn "Сравнение: ВЕРНО"
                else do
                    putStrLn "Сравнение: НЕВЕРНО"
                    mapM_ putStrLn diff

        (Left _, _) ->
            putStrLn "Ошибка: не найден output файл"

        (_, Left _) ->
            putStrLn "Ошибка: не найден exception файл"


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



makeOutputFile :: String -> Int -> String
makeOutputFile input mode =
    let (name, _) = break (== '.') input
        base = filter (`elem` ['0'..'9']) name
    in "output_" ++ base ++ "_" ++ show mode ++ ".txt"


makeExceptionFile :: String -> Int -> String
makeExceptionFile input mode =
    let (name, _) = break (== '.') input
        base = filter (`elem` ['0'..'9']) name
    in "exception_" ++ base ++ "_" ++ show mode ++ ".txt"