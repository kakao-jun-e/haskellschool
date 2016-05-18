myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs
-- myLast = head . reverse

elementAt :: [a] -> Int -> a
elementAt (x:xs) idx
  | idx == 0 = x
  | otherwise = elementAt xs (idx-1)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome [x] = True
isPalindrome (x:xs)
  | x == last xs = isPalindrome $ init xs
  | otherwise = False

compress :: (Eq a) => [a] -> [a]
compress [x] = [x]
compress (x:xs)
  | x == head xs = compress xs
  | otherwise = x:compress xs

encode :: (Eq a) => [a] -> [(Int, a)]
encode [x] = [(1, x)]
encode (x:xs)
  | x == y = (cnt+1, x):tail(encode xs)
  | otherwise = (1,x):encode xs
  where (cnt, y) = head $ encode xs
