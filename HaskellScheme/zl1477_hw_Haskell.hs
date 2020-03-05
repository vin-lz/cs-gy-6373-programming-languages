func :: [i1] -> [i2] -> [(i1, i2)]
func l1 l2 = func' l1 l2 
    where func' [] _ = []
          func' l1' [] = func' (tail l1') l2
          func' l1' l2' = (head l1', head l2') : func' l1' (tail l2')
