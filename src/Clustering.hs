module Clustering (
  centroid,
  distanceEucSquared,
  distanceEuc,
  inertia,
  wcss,
) where

import Data.List

centroid :: (Fractional a) => [[a]] -> [a]
centroid xs =
  let numValues = fromIntegral (length xs)
      numComponents = fromIntegral (length $ head xs)
      transposedValues = transpose xs
      componentAverages = map (\componentList -> sum componentList / numValues) transposedValues
   in componentAverages

distanceEucSquared :: (Num a, Floating a) => [a] -> [a] -> a
distanceEucSquared xs ys = sum $ zipWith ((-) . (^ 2)) xs ys

distanceEuc :: (Num a, Floating a) => [a] -> [a] -> a
distanceEuc xs ys = sqrt $ distanceEucSquared xs ys

inertia :: (Floating a) => [[a]] -> a
inertia xs =
  let mCentroid = centroid xs
   in sum $ map (distanceEucSquared mCentroid) xs

wcss :: (Floating a) => [[[a]]] -> a
wcss xs = sum $ map inertia xs
