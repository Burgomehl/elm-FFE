module Ex5 exposing (..)
import Result exposing (..)
import String exposing (..)
import Char exposing (..)
import List exposing (..)

type Base=
    U
    |A
    |C
    |G


type alias Rna = List Base


empty: Rna
empty = []


extend: Base -> Rna -> Rna
extend b r =
    r++singleton b

inverse: Rna -> Rna
inverse r =
    List.reverse r


baseChanger : Base -> Base
baseChanger b =
    case b of
        U -> A
        A -> U
        C -> G
        G -> C


komp: Rna -> Rna
komp r =
    case r of
        [] ->
            empty
        b :: x ->
            (baseChanger b) :: (komp x)


stringOfBase: Base -> String
stringOfBase b =
    case b of
        A -> "A"
        U -> "U"
        C -> "C"
        G -> "G"


toString: Rna -> String
toString r =
            String.concat (List.map (\x -> (stringOfBase x)) r)


parseBase: Char -> Result String Base
parseBase c =
  case Char.toUpper c of
    'A' -> Ok A
    'U' -> Ok U
    'G' -> Ok G
    'C' -> Ok C
    d -> Err ("Falsche Base: "++cons d "")


parse: String -> Result String Rna
parse s =
  case (uncons s) of
    Just (v, v2)->
      Result.map2 (::) (parseBase v) (parse v2)
    Nothing ->
      Ok []


testSequence = [U, A , C , G]
