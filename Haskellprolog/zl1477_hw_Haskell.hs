-- Q1. Write a program that finds the maximum of a list of numbers. Assume there are no duplicates in the list.
max :: Ord a => [a] -> a
max xs = head (maxSort xs)
    where maxSort [] = []
          maxSort (x : xs) = maxSort left ++ [x] ++ maxSort right
              where left = [y | y <- xs, y > x]
                    right = [y | y <- xs, y <= x]


-- Q2. Write a program that succeeds if the intersection of tow given list parameters is empty.
intersection :: (Foldable t, Eq a) => [a] -> t a -> Bool
intersection xs y = intersection' xs y
    where intersection' [] _ = True
          intersection' (x : xs) (y) = if (elem x y) then False
                                      else intersection' xs y


-- Q3. Write a program that returns a list containing the union of the elements of two given lists. Again, assume there are no duplicates in each list (although the same element might be in both lists).
union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ union' ys xs
    where union' [] _ = []
          union' (x:xs) (ys) = if (elem x ys) then union' xs ys
                               else x : union' xs (x : ys)


-- Q4. Write a program that returns the final element of a list
finalElement :: [a] -> a
finalElement [x] = x
finalElement (x : xs) = finalElement xs


-- Q5. Write a function that takes two lists of integers and returns a list containing tuples with corresponding elements from both lists.
carProduct :: [i1] -> [i2] -> [(i1, i2)]
carProduct l1 l2 = carProduct' l1 l2
    where carProduct' [] _ = []
          carProduct' l1' [] = carProduct' (tail l1') l2
          carProduct' l1' l2' = (head l1', head l2') : carProduct' l1' (tail l2')
