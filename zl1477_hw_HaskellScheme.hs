hw :: [i1] -> [i2] -> [(i1, i2)]
hw l1 l2 = hw' l1 l2 
    where hw' [] _ = []
          hw' l1' [] = hw' (tail l1') l2
          hw' l1' l2' = (head l1', head l2') : hw' l1' (tail l2')
