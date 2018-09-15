[[src]](https://github.com/ghc/ghc/tree/master/compiler/utils/Bag.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Bag: an unordered collection with duplicates


 Satisfy predictate 

 Don't 

 Left  

 Right 

 Standard definition
foldBag t u e EmptyBag        = e
foldBag t u e (UnitBag x)     = u x
foldBag t u e (TwoBags b1 b2) = (foldBag t u e b1) `t` (foldBag t u e b2)
foldBag t u e (ListBag xs)    = foldr (t.u) e xs
