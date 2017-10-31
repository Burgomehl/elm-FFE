module Exercise1 exposing (..)
import Debug exposing (..)
b = 5

ggT: Int -> Int -> Int
ggT n m =
    if n==0 then
        m
    else if m==0 then
        n
    else if m>n then
        ggT (m - n) n
    else
        ggT m (n - m)

kgV: Int -> Int -> Int
kgV a b = (a*b)//(ggT a b)

sumN: Int -> Int
sumN n =
    if n<0 then
        -1
    else
        case n of
            0 ->
                0
            m ->
                n + sumN (n - 1)

fakN: Int -> Int
fakN n =
     if n<0 then
         -1
     else
         case n of
             0 ->
                 0
             m ->
                 n * sumN (n - 1)

endSumN: Int -> Int -> Int
endSumN n m =
    log (toString m) (if n==0 then
        m
    else
        endSumN (n-1) (n+m))

endFakN: Int -> Int -> Int
endFakN n m =
     if n<=0 then
             m
         else
             endFakN (n-1) (n*m)

sigNum: Int -> Int
sigNum n =
    if n == 0 then
        0
    else if n > 0 then
        1
    else
        -1