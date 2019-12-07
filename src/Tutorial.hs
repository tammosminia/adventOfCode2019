doubleMe x = x + x

som :: (Integral a) => [a] -> a
som [] = 0
som xs = (head xs) + (som (tail xs))

draai :: [a] -> [a]
draai [] = []
draai [x] = [x]
draai (h : t) = (draai t) ++ [h]
