module Loss (
  maeLoss,
  mseLoss,
  zeroOneLoss,
) where

normLoss :: (Fractional c, Num a) => (a -> c) -> [a] -> [a] -> c
normLoss f xs ys =
  let mean = (/ fromIntegral (length xs))
      err = zipWith (-) xs ys
   in mean . sum . map f $ err

maeLoss :: (Num a, Fractional a) => [a] -> [a] -> a
maeLoss = normLoss abs

mseLoss :: (Num a, Fractional a) => [a] -> [a] -> a
mseLoss = normLoss (^ 2)

zeroOneLoss :: (Eq a) => [a] -> [a] -> Integer
zeroOneLoss xs ys = sum $ zipWith (\a b -> if a == b then 0 else 1) xs ys
