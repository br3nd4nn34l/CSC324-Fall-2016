data MaybeInt = Success Integer | Failure deriving Show

isSuccess :: MaybeInt -> Bool
isSuccess (Success x) = True
isSuccess (Failure) = False

isFailure :: MaybeInt -> Bool
isFailure (Success x) = False
isFailure (Failure) = True

getValue :: MaybeInt -> Integer -> Integer
getValue (Success x) y = x
getValue (Failure) y = y

collectSuccesses :: [MaybeInt] -> [Integer]
collectSuccesses [] = []
collectSuccesses (x:xs) = if isSuccess x then 
                             (getValue x 1):(collectSuccesses xs) 
                          else (collectSuccesses xs)

addMaybe :: MaybeInt -> MaybeInt -> MaybeInt
addMaybe (Success x) (Success y) = (Success (x + y))
addMaybe _ _ = Failure
