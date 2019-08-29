import Test.QuickCheck
import Test.Hspec
import qualified Numbers

 
genNumbers :: Gen (Int,Int)
genNumbers = choose 

-- >>> sample' genNumbers 
-- <interactive>:676:2-8: error:
--     Variable not in scope: sample' :: t0 -> t
-- <BLANKLINE>
-- <interactive>:676:10-19: error: Variable not in scope: genNumbers
--

prop_convertNumber :: Property
prop_convertNumber =
  forAll genNumbers
    (\(x,y)-> lcm x y == Numbers.leastCommonMultiple x y)


main :: IO ()
main = hspec $ do
  describe "LCM Parity:" $ do
      it "leastCommonMultiple = lcm" $ property $
        prop_convertNumber