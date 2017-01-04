x :: [Char]
x = if 3 > 4 then "Goodbye" else "Hello"

y :: [Integer]
y = [4,5,6,7]

z :: [[Bool]]
z = [[],[null [], False, True, 3 > 4], [True]]

-- :t f --> f :: (Conditions such that f can be executed) => InpType1 -> InpType2 -> ... -> OutType

f :: (Num a, Ord a) => a -> [Char]
f x = if x + 3 > 10 then "Hi" else "Bye"

g :: Num a => [a] -> a
g x = (head x) + 10

h :: t -> [t]
h x = [x]

j :: (Num a, Ord a) => a -> a -> Bool
j x y = x > (y + 1)

j1 :: Integer -> Bool
j1 = j 2

k :: Bool -> t -> t -> t
k x y z = if x then y else z

k1 :: [Char] -> [Char]
k1 = k True "Hi"

applyToThree :: Num a => (a -> t) -> t
applyToThree f = f 3

apply :: (a -> t) -> a -> t
apply f x = f x

makeAdder :: Num a => a -> (a -> a)
makeAdder x = \y -> x + y

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x):(map f xs)

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) = if (f x) then x:(filter f xs) else (filter f xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f i [] = i
foldl f i (x:xs) = (foldl f (f i x) xs)
