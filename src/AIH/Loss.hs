module AIH.Loss (
    maeLoss,
    mseLoss,
    zeroOneLoss
) where 

maeLoss :: (Num a, Fractional a) => [a] -> [a] -> a
maeLoss xs ys = (\n -> n / fromIntegral (length xs)) $ sum $ map abs $ zipWith (-) xs ys

mseLoss :: (Num a, Fractional a) => [a] -> [a] -> a
mseLoss xs ys = (\n -> n / fromIntegral (length xs)) $ sum $ map (\n -> n * n) $ zipWith (-) xs ys

zeroOneLoss :: (Eq a) => [a] -> [a] -> Integer
zeroOneLoss xs ys = sum $ zipWith (\a b -> if a == b then 0 else 1) xs ys