-- Polynomial multiplication
-- polynomial representation in tuples of (coefficient, exponent)
poly1=[(6,1),(2,2),(3,3),(1,4)]
poly2=[(1,2)]
-- filters all elem of a list of tuples at the snd place for a pattern 
maphelp [] = []
maphelp [x] = []
maphelp l = l
maptuple k (x:xs) =  maphelp (filter (\x -> k == snd x) (x:xs))
-- multiply each coefficient in poly1 with each coefficient in poly2 and add exponents
mult :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
mult  [] [] = []
mult lst1 lst2 = head(map (\a -> (map (\x -> ((fst x)*(fst a),(snd x)+(snd a))) lst1)) lst2)

-- desired result: [(6,3),(2,4),(3,5),(1,6)]
-- main = mapM_ print (pmultify poly1 poly2)
