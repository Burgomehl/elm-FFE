module Ex4 exposing(..)
import Maybe exposing(..)

type  List a
  = Nil
  | Const a (List a)


reverse: List a -> List a
reverse l =
  case l of
    Nil ->
      Nil
    Const i xl->
      extend i (reverse xl)


reverseE: List a -> List a
reverseE l1 =
  reverseEnd l1 Nil


reverseEnd: List a -> List a -> List a
reverseEnd l1 l2 =
  case l1 of
    Nil ->
      l2
    Const i xl ->
      reverseEnd xl (Const i l2)


extend: a -> List a -> List a
extend y x =
  case x of
    Nil ->
      Const y Nil
    Const a xl ->
      Const a (extend y xl)


last: List a -> Maybe a
last l1 =
  case l1 of
    Nil ->
      Nothing
    Const a Nil ->
      Just a
    Const a xl ->
      last xl


--indexOf mit Hilfsmethode
indexOf: List String -> String -> Maybe Int
indexOf l1 s =
  indexOfR l1 s 1


indexOfR: List String -> String -> Int -> Maybe Int
indexOfR l1 s n =
  case l1 of
    Nil ->
      Nothing
    Const a xl ->
      if a == s then
        Just (n)
      else
        indexOfR xl s (n + 1)


--alternative Idee (ohne Hilfsmethode) ...
--zweiter case könnte ausgelagert werden
alternateIndexOf: List String -> String -> Maybe Int
alternateIndexOf l1 s =
  case l1 of
    Nil ->
      Nothing
    Const a xl ->
      if a == s then
        Just 1
      else
        case (alternateIndexOf xl s ) of
          Just v ->
            Just (v + 1)
          Nothing ->
            Nothing


content = 2 :: 3 :: 4 :: 5 :: 6 :: []
content2 = (Const 2 (Const 3 (Const 4 Nil)))
content3 = Nil
content4 = (Const "Flensburg" (Const "Hamburg" (Const "Bremen" (Const "Berlin" Nil))))


