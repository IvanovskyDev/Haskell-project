module Validate(validateNKA) where

import Types
import Data.List (nub)

validateNKA :: NKA -> Either [String] ()
validateNKA nka
  | null errs = Right ()
  | otherwise = Left errs
 where
  states = nkaStates nka

  errs =
      startErrors
   ++ finalErrors
   ++ transErrors
   ++ dupErrors

  startErrors =
      ["Начальное состояние отсутствует во множестве состояний"
      | nkaStart nka `notElem` states]

  finalErrors =
      [ "Конечное состояние " ++ unState s ++ " отсутствует"
      | s <- nkaFinal nka
      , s `notElem` states
      ]

  transErrors =
      [ "Некорректный переход "
          ++ unState f ++ " -> " ++ unState t
      | (f,_,t) <- nkaTransitions nka
      , f `notElem` states || t `notElem` states
      ]

  dupErrors =
      ["Есть повторяющиеся переходы"
      | length (nub (nkaTransitions nka))
          /= length (nkaTransitions nka)
      ]