-- Lab 4: Introduction to Haskell

double x = x + x

sumOfSquares x y = x*x + y*y

absVal x = if x >= 0 then x else -x

lengthPlus3 [] = 3
lengthPlus3 (x:xs) = 1 + (lengthPlus3 xs)

applyUnary fxn val = (fxn val)

makeConstantFunction x = (\y -> x)

join [] = ""
join (x:xs) = x ++ (join xs)

listReverse [] = []
listReverse (x:xs) = (listReverse xs) ++ [x]

myMap fxn [] = []
myMap fxn (x:xs) = [fxn x] ++ (myMap fxn xs)

myFilter fxn [] = []
myFilter fxn (x:xs) = (if (fxn x) then [x] else []) ++ (myFilter fxn xs)
