import Test.HUnit

main =
    runTestTT (TestCase $ do assertEqual "hey" (1+1) 2)