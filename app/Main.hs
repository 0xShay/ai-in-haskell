module Main (main) where

import Clustering
import LinRegression
import Loss

testLearningRate = 0.05
testParams = Point 0 0
testDataSet = [Point 0 0, Point 1 2, Point 2 4, Point 4 8, Point 8 16]

main :: IO ()
main = do
  let trainedParams = uniLinRegression testLearningRate 0.01 testDataSet testParams
  print trainedParams
  print (maeLoss [1, 2, 3] [1, 6, 3])
  print (mseLoss [1, 2, 3] [1, 6, 3])
  print (zeroOneLoss [1, 2, 3] [1, 6, 3])
  let clusteringStructure = [[[1, 2], [22, 3], [3, 4]], [[64, 5], [5, 6], [6, 7]]]
  print (centroid (head clusteringStructure))
  print (distanceEuc [1, 2, 3] [2, 4, 6])
  print (inertia (head clusteringStructure))
  print (wcss clusteringStructure)
