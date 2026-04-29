module Output
    ( formatGrammar
    , formDKA
    ) where

import Types
import Data.List (intercalate)

-- грамматика в строку
formatGrammar :: Grammar -> String
formatGrammar g =
    "Нетерминалы: " ++ intercalate "," (map unState (grammarNonTerminals g)) ++ 
        "\n" ++
    "Терминалы: " ++ intercalate "," (map (:[]) (grammarTerminals g)) ++
        "\n\n" ++ rulesText (grammarRules g)

-- правила в строку
rulesText :: [(State, [Rules])] -> String
rulesText [] = ""
rulesText ((s,rs):xs) = unState s ++ "=" ++ 
    intercalate "|" (map showRule rs) ++ ";\n" ++ rulesText xs

showRule :: Rules -> String
showRule (Terminal a) = [a]
showRule (RightGrammar a s) = [a] ++ unState s
showRule (LeftGrammar s a) = unState s ++ [a]
showRule (NonTerminal s) = unState s
showRule Epsilon = "ε"


-- дка в строку
formDKA :: DKA -> String
formDKA dka =
    "Состояния: " ++ intercalate "," (map unState (dkaStates dka)) ++ "\n" ++
    "Начальное: " ++ unState (dkaStart dka) ++ "\n" ++
    "Конечные: " ++ intercalate "," (map unState (dkaFinal dka)) ++ "\n\n" ++
    transText (dkaTransitions dka)

-- переходы в строку
transText :: [((State, Char), State)] -> String
transText [] = ""
transText (((s,a),t):xs) = unState s ++ " --" ++ [a] ++ 
            "--> " ++ unState t ++ "\n" ++ transText xs