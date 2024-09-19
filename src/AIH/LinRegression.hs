module AIH.LinRegression (
  Point (..),
  uniLinRegression,
) where

import AIH.Loss (mseLoss)
import Debug.Trace (trace)

data Point = Point Float Float deriving (Show, Eq)

type X = Float
type Y = Float
type Params = Point
type Dataset = [Point]

xPoint :: Point -> X
xPoint (Point x _) = x
yPoint :: Point -> Y
yPoint (Point _ y) = y

predictedY :: Params -> X -> Y
predictedY (Point w0 w1) x = w0 + x * w1

predictedSet :: Params -> Dataset -> [Y]
predictedSet params = map $ predictedY params . xPoint

costDataset :: Params -> Dataset -> Float
costDataset params dataset = mseLoss (predictedSet params dataset) (map yPoint dataset)

costChanged :: Float -> Params -> Params -> Dataset -> Bool
costChanged thresh oldParams newParams dataset =
  let oldCost = costDataset oldParams dataset
      newCost = costDataset newParams dataset
   in abs (oldCost - newCost) > thresh

dw0 :: Params -> Point -> Float
dw0 params (Point x y) = predictedY params x - y
dw1 :: Params -> Point -> Float
dw1 params point@(Point x y) = dw0 point params * x

-- TODO: this sux
updateParamsDataset :: Float -> Params -> Dataset -> Params
updateParamsDataset rate params@(Point w0 w1) dataset =
  let newW0 = w0 - (((2 * rate) / fromIntegral (length dataset)) * sum (map (dw0 params) dataset))
      newW1 = w1 - (((2 * rate) / fromIntegral (length dataset)) * sum (map (dw1 params) dataset))
   in Point newW0 newW1

uniLinRegression :: Float -> Float -> Dataset -> Params -> Params
uniLinRegression a thresh dataset params | trace ("regress " ++ show params) False = undefined
uniLinRegression a thresh dataset params
  | not $ costChanged thresh params newParams dataset = params
  | otherwise = uniLinRegression a thresh dataset newParams
 where
  newParams = updateParamsDataset a params dataset

-- test
main :: IO ()
main = do
  let testLearningRate = 0.01
  let testParams = Point 0 0
  let testDataSet = [Point 1 2, Point 2 4, Point 3 6, Point 4 8, Point 5 10]
  let trainedParams = uniLinRegression testLearningRate 0.001 testDataSet testParams
  print trainedParams
