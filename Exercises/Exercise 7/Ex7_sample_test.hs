import Test.HUnit
import Ex7 (domain, TripleDeep(..))

domainTests :: Test
domainTests = TestList [
    [2,1] ~=? domain [(2, 1), (1, 2)],
    [1] ~=? domain [(1, 2), (1, 3), (1, 4)],
	[1,2] ~=? domain [(1, 2), (2, 3), (1, 3)]
    ]

t1 = ShallowEnd 1
s1 = ShallowEnd "1"
t2 = DeepEnd (ShallowEnd 1) (DeepEnd (ShallowEnd 5) (ShallowEnd 2) (ShallowEnd 3)) (DeepEnd (ShallowEnd 1) (ShallowEnd 2) (ShallowEnd 3))
s2 = DeepEnd (ShallowEnd "1") (DeepEnd (ShallowEnd "5") (ShallowEnd "2") (ShallowEnd "3")) (DeepEnd (ShallowEnd "1") (ShallowEnd "2") (ShallowEnd "3"))

tdTests :: Test
tdTests = TestList [
    s1 ~=? fmap show t1,
    s2 ~=? fmap show t2
    ]

main :: IO ()
main = do
    runTestTT domainTests
    runTestTT tdTests
    return ()