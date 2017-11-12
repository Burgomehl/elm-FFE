module Ex6IntSet exposing (..)

type alias IntSet = Int -> Bool

empty: IntSet
empty = \x -> False


isElem: IntSet -> Int -> Bool
isElem s i =
    case s of
        (e, x) ->
            if e == i then
                True
            else
                False




singleton: Int -> IntSet
singleton i =
    \x -> i == x


insert: Int -> IntSet -> IntSet
insert i iS =
    (\x -> iS) i

