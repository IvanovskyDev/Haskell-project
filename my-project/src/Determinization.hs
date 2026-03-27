module Determinization(
    isDetermin,
    determinize
) where

import Types
import Data.List(nub)

-- если нет переходов по eps и по каждому символу не более одного перехода => детерм.
isDetermin :: NKA -> Bool
isDetermin nka = noEps && all (\(s,a) -> length(transFrom s a)<=1) pairs
    where trans = nkaTransitions nka
          noEps = not (any (\(_,t,_) -> t == Eps) trans)
          pairs = nub [(f,a) | (f,Term a,_) <- trans]
          transFrom s a = [t | (f,Term b,t) <- trans, f==s,b==a]


-- один шаг eps-замыкания
epsStep :: [State] -> [(State, Terminal, State)] -> [State]
epsStep xs trans = nub(xs ++ [t | s <- xs, (f, Eps, t) <- trans, f==s])

-- полное eps-замыкание
epsClosure :: [State] -> [(State, Terminal, State)] -> [State]
epsClosure states trans = closure states
    where closure xs = if length next == length xs
                       then nub $ xs
                       else closure next
                    where
                        next = epsStep xs trans


-- переход из множества по символу без eps-замыкания
move :: [State] -> Char -> [(State, Terminal, State)] -> [State]
move states a trans = nub [t | s <- states, (f,Term b,t) <- trans, f==s, b==a]


-- все множества, достижимые из данного множества по одному символу
nextStates :: [State] -> [(State, Terminal, State)] -> [Char] -> [[State]]
nextStates s trans alphabet = [epsClosure(move s a trans) trans | a <- alphabet]

-- построение всех достижимых множеств
buildStates :: [[State]] -> [[State]] -> [(State, Terminal, State)] -> [Char] -> [[State]]
buildStates [] visited _ _ = visited
buildStates (s:ss) visited trans alph
    | elem s visited = buildStates ss visited trans alph
    | otherwise = buildStates (ss++nextStates s trans alph) (visited++[s]) trans alph


-- имя множества состояний
setName :: [State] -> State
setName [] = State "∅"
setName ss = State (concatMap unState (nub ss))

-- детерминизация
determinize :: NKA -> DKA
determinize nka = DKA{
    dkaStates      = map setName allSets,
    dkaTerminals   = alphabet,
    dkaTransitions = concatMap (\s -> map (\a ->((setName s,a),setName(nextSet s a))) alphabet) allSets,
    dkaStart       = setName startSet,
    dkaFinal       = map setName [s | s <- allSets, any (`elem` nkaFinal nka) s]
    }
    where
        alphabet = nkaTerminals nka
        trans = nkaTransitions nka
        startSet = epsClosure [nkaStart nka] trans
        allSets = buildStates [startSet] [] trans alphabet
        nextSet s a = epsClosure (move s a trans) trans
