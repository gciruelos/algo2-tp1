module Set
        ( Set (Vacio, Ag)
        , esVacio
        , en
        , (-:)
        , card
        , sinUno, dameUno
        , union
        ) where


data Set a = Vacio | Ag a (Set a)

esVacio Vacio = True
esVacio (Ag _ _) = False

en :: Eq a => a -> Set a -> Bool
a `en` Vacio = False
a `en` (Ag x as) = a == x || a `en` as


(-:) :: Eq a => Set a -> a -> Set a
Vacio -: a = Vacio
(Ag x as) -: a = if x == a then as -: a else Ag x (as -: a)


card Vacio = 0
card (Ag _ c) = 1+(card c)

dameUno (Ag x _) = x
sinUno s = s -: (dameUno s)

instance Eq a => Eq (Set a) where
  Vacio == Vacio  = True
  (Ag x xs) == ys = (xs -: x) == (ys -: x)
  xs == (Ag y ys) = (ys -: y) == (xs -: y)

toList :: Eq a => Set a -> [a]
toList Vacio = []
toList (Ag x xs) = x:(toList (xs-:x))

instance (Eq a, Show a) => Show (Set a) where
  show c = "{"++((init.tail.show.toList)c) ++ "}"


Vacio `union` xs = xs
xs `union` Vacio = xs
(Ag x xs) `union` c = xs `union` (Ag x c)


