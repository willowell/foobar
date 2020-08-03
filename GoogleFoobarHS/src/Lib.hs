module Lib where

-- Pairs each element of a list with a number, starting from n
-- use fst to access the value
-- and snd to access the index value
-- ys: range for enumeration [0..], [1..], etc.
-- xs: the list to enumerate over
enumerate :: [b] -> [a] -> [(a, b)]
enumerate ys xs = zip xs ys
{-
Note: As a consequence of this type signature, you can zip whatever you like, e.g.,
GHCi> enumerate ['a'..'z'] [1..26]
[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),(8,'h'),(9,'i'),(10,'j'),
(11,'k'),(12,'l'),(13,'m'),(14,'n'),(15,'o'),(16,'p'),(17,'q'),(18,'r'),(19,'s'),
(20,'t'),(21,'u'),(22,'v'),(23,'w'),(24,'x'),(25,'y'),(26,'z')]
-}

enum0 :: (Enum b, Num b) => [a] -> [(a, b)]
enum0 = enumerate [0..]

enum1 :: (Enum b, Num b) => [a] -> [(a, b)]
enum1 = enumerate [1..]