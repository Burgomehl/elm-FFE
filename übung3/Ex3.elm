module Ex3 exposing (..)

type Base=
    U |
    A |
    C |
    G


type Rna =
     Nil
    | Const Base Rna


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

testSequence = Const U (Const A (Const C (Const G empty)))