module Ex4Rna exposing (..)
import Result exposing (..)
import String exposing (..)

type Base=
    U
    |A
    |C
    |G


type Rna =
     Nil
    | Const Base Rna


empty: Rna
empty = Nil


extend: Base -> Rna -> Rna
extend b r =
    case r of
        Nil ->
            Const b empty
        Const b1 rna ->
            Const b1 (extend b rna)


inverse: Rna -> Rna
inverse r =
    case r of
        Nil ->
            empty
        Const b x ->
            extend b (inverse x)


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
        Nil ->
            empty
        Const b x ->
            Const (baseChanger b) (komp x)


stringOfBase: Base -> String
stringOfBase b =
    case b of
        A -> "A"
        U -> "U"
        C -> "C"
        G -> "G"


toString: Rna -> String
toString r =
    case r of
        Nil ->
            ""
        Const b x ->
            stringOfBase b++toString x



parseBase: Char -> Result String Base
parseBase c =
  case c of
    'a' -> Ok A
    'A' -> Ok A
    'u' -> Ok U
    'U' -> Ok U
    'g' -> Ok G
    'G' -> Ok G
    'c' -> Ok C
    'C' -> Ok C
    d -> Err ("Falsche Base: "++cons d "")


parse: String -> Result String Rna
parse s =
  case (uncons s) of
    Just (v, v2)->
      case (parseBase v) of
        Ok c ->
          case (parse v2) of
            Ok v3 ->
              Ok (Const c v3)
            Err d ->
              Err d
        Err d ->
          Err d
    Nothing ->
      Ok Nil


testSequence = Const U (Const A (Const C (Const G empty)))
