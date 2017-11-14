module Ex6fold exposing (..)

import List exposing (..)

{- foldl ist so definiert, dass es die Liste umdreht.-}
reverse: List a -> List a
reverse l =
    List.foldl (::) [] l

{- foldr behält die Reihenfolge. Hier ist die Reihenfolge relevant -}
concat: List a -> List a -> List a
concat l1 l2 =
    List.foldr (::) l1 l2

{- auch hier foldr, damit die Reihenfolge gleich bleibt, es sollen ja nur die Werte geändert werden und nicht die Reihenfolge -}
map: (a->b) -> List a -> List b
map f l =
    List.foldr (\x xs-> f(x)::xs) [] l

{- auch hier würde ich die Reihenfolge als relevant ansehen -> daher foldr -}
filter: (a -> Bool) -> List a -> List a
filter f l =
    List.foldr (\x xs -> if f(x) then x::xs else xs) [] l


{- auch hier würde ich die Reihenfolge als relevant ansehen -> daher foldr -}
unzip: List (a,b) -> (List a, List b)
unzip l =
    foldr (\(a, b) (xs1,xs2) -> (a::xs1,b::xs2)) ([],[]) l


xor2 : List Bool -> Bool
xor2 l =
    List.length (List.filter(\x -> x == True) l)%2 == 0

xor: List Bool -> Bool
xor l =
    (l
    |> List.filter(\x->x==True)
    |> List.length) %2 == 0
