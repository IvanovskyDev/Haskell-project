module Grammar(
    rightGrammar,
    leftGrammar
) where

import Types
import Data.List(nub)

rightGrammar :: NKA -> Grammar
rightGrammar nka = Grammar {
    grammarNonTerminals = nkaStates nka,
    grammarTerminals    = nkaTerminals nka,
    grammarStart        = nkaStart nka,
    grammarRules        = groupRules (nkaStates nka) (makeRightRules 
    (nkaTransitions nka) ++ epsilonRules),
    grammarLeft         = False
    }
  where
    epsilonRules = [(s,Epsilon) | s <- nkaFinal nka]

makeRightRules :: [(State, Terminal, State)] -> [(State, Rules)]
makeRightRules [] = []
makeRightRules ((f,s,t):xs) = case s of
    Term c -> (f, RightGrammar c t) : makeRightRules xs -- A->aB
    Eps    -> (f, NonTerminal t) : makeRightRules xs -- A->B


leftGrammar :: NKA -> Grammar
leftGrammar nka = Grammar{ 
    grammarNonTerminals = nkaStates nka,
    grammarTerminals    = nkaTerminals nka,
    grammarStart        = State "S",
    grammarRules        = groupRules (nkaStates nka) rules,
    grammarLeft         = True
    }
    where
      rules = makeLeftRules (nkaTransitions nka) ++ [(f,Epsilon)| f <- nkaFinal nka]

makeLeftRules :: [(State, Terminal, State)] -> [(State, Rules)]
makeLeftRules [] = []
makeLeftRules ((f,s,t):xs) = case s of
    Term c -> (t, LeftGrammar f c) : makeLeftRules xs -- A->Ba
    Eps    -> (t, NonTerminal f) : makeLeftRules xs --B->A

groupRules :: [State] -> [(State, Rules)] -> [(State, [Rules])]
groupRules [] _ = []
groupRules (s:ss) rules = case collect s rules of
    [] -> groupRules ss rules
    rs -> (s, nub rs) : groupRules ss rules

collect :: State -> [(State,Rules)] -> [Rules]
collect _ [] = []
collect s ((l,r):xs) | s==l = nub(r:collect s xs)
                     | otherwise = collect s xs