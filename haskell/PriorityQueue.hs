module PriorityQueue
        ( PQ (Empty, Enqueue)
        , vacia
        , proxima
        , desencolar
        
        ) where

data PQ a = Empty | Enqueue a (PQ a)

vacia Empty = True
vacia (Enqueue _ _) = False

proxima (Enqueue e c) = if vacia c || (proxima c) < e then e else proxima c

desencolar (Enqueue e c) = if vacia c || (proxima c) < e then c else Enqueue e (desencolar c)

