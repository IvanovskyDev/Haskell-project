module Runner (run) where

import Parser (parseTransitions)
import Grammar (rightGrammar, leftGrammar)
import Determinization (determinize, isDetermin)
import Output (formatGrammar, formDKA)
import Types (State(..), Transition(..), Terminal(..), NKA(..), Grammar(..))
import Data.List (nub)

run :: IO()
run = do
    putStrLn "Введите имя файла: "
    path <- getLine
    contents <- readFile path
    case parseTransitions (lines contents) of
        Left err -> do
            putStrLn ("Ошибка парсинга: " ++ err)
            return ()
        Right trans -> do
            putStrLn "\nВыберите тип грамматики:"
            putStrLn "0 - праволинейная (по умолчанию)"
            putStrLn "1 - леволинейная"
            putStrLn "Ваш выбор (0/1): "
            typ <- getLine
            process (buildNKA trans) typ

process :: NKA -> String -> IO()
process nka typ = do
    putStrLn "\nРегулярная грамматика:"
    putStrLn $ formatGrammar (selectGrammar nka typ)
    if isDetermin nka
        then putStrLn "\nАвтомат детерминированный, ДКА не требуется."
        else do
            putStrLn "\nЭквивалентный детерминированный автомат:"
            putStrLn $ formDKA (determinize nka)

selectGrammar :: NKA -> String -> Grammar
selectGrammar nka typ = case typ of
                "1" -> leftGrammar nka
                _   -> rightGrammar nka

buildNKA :: [Transition] -> NKA
buildNKA trans = NKA{ 
    nkaStates      = states,
    nkaTerminals   = alphabet,
    nkaTransitions = map (\t -> (from t,onTerm t,to t)) trans,
    nkaStart       = State "H",
    nkaFinal       = [State "S"]
    }
    where
        allStates = nub(concatMap (\t -> [from t,to t]) trans)
        states = nub(allStates ++ [State "H",State "S"])
        alphabet = nub [c | Transition _ (Term c) _ <- trans]