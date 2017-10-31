module ST exposing (..)
import List exposing (..)

type SearchTree a
    = Leaf
    | Node (SearchTree a) Int a (SearchTree a)

insert: SearchTree a -> a -> Int -> SearchTree a
insert tree value sKey =
    case tree of
        Leaf ->
            Node Leaf sKey value Leaf
        Node left currentSKey cValue right ->
            if sKey < currentSKey then
                Node (insert left value sKey) currentSKey cValue right
            else
                if sKey == currentSKey then
                    Node left sKey value right
                else
                    Node left currentSKey cValue (insert right value sKey)

lookupSearchTree: SearchTree a -> Int -> Maybe a
lookupSearchTree tree sKey =
    case tree of
        Leaf ->
            Nothing
        Node left csKey value right ->
            if csKey == sKey then
                Just value
            else
                if sKey < csKey then
                    lookupSearchTree left sKey
                else
                    lookupSearchTree right sKey


searchMinNode: SearchTree a -> SearchTree a
searchMinNode tree  =
    case tree of
        Leaf ->
            Leaf
        Node Leaf key value _ ->
            tree
        Node left key value _ ->
            searchMinNode left


delete: SearchTree a -> Int -> SearchTree a
delete tree sKey =
    case tree of
        Leaf ->
            Leaf
        Node Leaf cSKey cValue right ->
            if cSKey == sKey then
                right
            else
                Node Leaf cSKey cValue (delete right sKey)
        Node left cSKey cValue Leaf ->
            if cSKey == sKey then
                left
            else
                Node (delete left sKey) cSKey cValue Leaf
        Node left cSKey cValue right ->
            if cSKey == sKey then
                case searchMinNode right of
                    Leaf ->
                        Leaf
                    Node l k v r ->
                        Node left k v (delete right k)
            else
                if sKey < cSKey then
                    Node (delete left sKey) cSKey cValue right
                else
                    Node left cSKey cValue (delete right sKey)


listToSearchTree: List (Int,a) -> SearchTree a
listToSearchTree l =
    case l of
        [] ->
            Leaf
        (k,v) :: xs ->
            insert (listToSearchTree xs) v k


searchTreeToList : SearchTree a -> List (Int, a)
searchTreeToList tree =
    case tree of
        Leaf ->
            []
        Node left k v right ->
            (searchTreeToList left)++[(k,v)]++(searchTreeToList right)


mapSearchTree: (a -> b) -> SearchTree a -> SearchTree b
mapSearchTree f tree =
    case tree of
        Leaf ->
            Leaf
        Node l k v r ->
            Node (mapSearchTree f l) k (f v) (mapSearchTree f r)


liste: List (Int, String)
liste = [(50,"Benny"),(20,"Annika"),(60,"Matthias"),(80,"Rüdiger"),(1,"Jan"),(30,"Sascha"),(55,"Laura")]