import LinRegression
import Test.HUnit

learningRate :: Float
learningRate = 0.01
learningThresh :: Float
learningThresh = 0.01
initialRegParams :: Point
initialRegParams = Point 0 0
testPoints1 :: [Point]
testPoints1 = [Point 0 0, Point 1 2, Point 2 4, Point 4 8, Point 8 16]

-- XXX: Dubious accuracy
roundPoint :: Point -> Point
roundPoint p = let roundFromFloat = fromIntegral . round in Point ((roundFromFloat . xPoint) p) ((roundFromFloat . yPoint) p)

main :: IO Counts
main = do
  let test1 = TestCase (assertEqual "a" (Point 0 2) (roundPoint (uniLinRegression learningRate learningThresh testPoints1 initialRegParams)))
  let tests = TestList [TestLabel "regression_test1" test1]
  runTestTT tests
