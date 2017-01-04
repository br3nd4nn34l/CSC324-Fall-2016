import Test.HUnit
import Ex8 (geoSeq, monadIf, monadIf2)

geoSeqTests :: Test
geoSeqTests = TestList [
    [3, 6, 12, 24] ~=? take 4 (geoSeq 3 2),
    3 * 1024 ~=? (geoSeq 3 2 !! 10)
    ]


monadIfTests :: Test
monadIfTests = TestList [
    Just 10 ~=? monadIf (Just True) (Just 10) (Just 20),
    Just 20 ~=? monadIf (Just False) (Just 10) (Just 20),
    -- Note: "bind" for Maybe will completely ignore its
    -- second argument if its first one is "Nothing"
    Nothing ~=? monadIf Nothing (Just 10) (Just 20)
    ]

monadIf2Tests :: Test
monadIf2Tests = TestList [
    Just (Left 10) ~=? monadIf2 (Just True) (Just 10) (Just 'a'),
    Just (Right 'a') ~=? monadIf2 (Just False) (Just 10) (Just 'a'),
    Nothing ~=? monadIf2 Nothing (Just 10) (Just 'a')
    ]


main :: IO ()
main = do
    printCount "geoSeq" geoSeqTests
    printCount "monadIf" monadIfTests
    printCount "monadIf2" monadIf2Tests


-- Test runner helpers. You are not responsible for this code.
justGetCount :: Test -> IO Counts
justGetCount tests = fmap fst (runTestText (PutText (\_ _ () -> return ())  ()) tests)

printCount :: String -> Test -> IO ()
printCount name tests = do
        count <- justGetCount tests
        putStrLn (name ++ ": " ++ show count)
