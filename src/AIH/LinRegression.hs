module AIH.LinRegression (
  Point (..),
  uniLinRegression,
) where

import Debug.Trace

data Point = Point Float Float deriving (Show)

updateW0 :: Float -> Point -> Point -> Float
updateW1 :: Float -> Point -> Point -> Float
updateW0 a (Point w0 w1) (Point xn yn) = w0 - (a * ((w1 * xn) + w0 - yn))
updateW1 a (Point w0 w1) (Point xn yn) = w1 - (a * ((w1 * xn) + w0 - yn) * xn)

updateParamsPoint :: Float -> Point -> Point -> Point
updateParamsPoint a params@(Point w0 w1) datapoint =
  let newW0 = updateW0 a params datapoint
      newW1 = updateW1 a params datapoint
   in Point newW0 newW1

hasChanged :: Float -> Point -> Point -> Bool
hasChanged thresh (Point w0 w1) (Point newW0 newW1) = (abs (newW0 - w0) + abs (newW1 - w1)) > thresh

updateParamsDataset :: Float -> Point -> [Point] -> Point
updateParamsDataset a = foldr (updateParamsPoint a)

uniLinRegression :: Float -> Float -> [Point] -> Point -> Point
uniLinRegression a thresh dataset params | trace ("regress " ++ show params) False = undefined
uniLinRegression a thresh dataset params
  | not $ hasChanged thresh params newParams = params
  | otherwise = uniLinRegression a thresh dataset newParams
 where
  newParams = updateParamsDataset a params dataset

-- test
-- TODO: Doesn't currently work because the slides from our university are plain incorrect about the algorithm
-- need to correct updateParamsDataset later
main :: IO ()
main = do
  let testLearningRate = 0.05
  let testParams = Point 0 0
  let testDataSet = [Point 0 0, Point 1 2]
  let trainedParams = uniLinRegression testLearningRate 0.01 testDataSet testParams
  print trainedParams
